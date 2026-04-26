/*
 * native.c - dynamic native code generation
 *
 *   Copyright (c) 2021-2025  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* EXPERIMENTAL */

/* CAUTION
 *   Be careful not to create a code path to call these APIs directly
 *   from Scheme.  It would open up for casual Scheme code to execute
 *   arbitrary machine code, bypassing our runtime check completely.
 *
 *   Instead, the Scheme API for these functionalities should be placed in
 *   gauche.bootstrap module.  The module is only alive during initialization
 *   process, so the built-in Scheme procedures can call them, but they'll
 *   be unaccessible from the user code.  See libnative.scm.
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/configP.h"
#include "gauche/priv/vmP.h"
#include "gauche/priv/mmapP.h"
#include "gauche/priv/nativeP.h"
#include "gauche/priv/typeP.h"

#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif

static long sys_getpagesize()
{
#if defined(GAUCHE_WINDOWS)
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return (long)sysinfo.dwPageSize;
#else  /* !GAUCHE_WINDOWS */
    return sysconf(_SC_PAGESIZE);
#endif /* !GAUCHE_WINDOWS */
}

/*======================================================================
 * FFI support
 */

/*
 * For the time being, we use a fixed area (mmapped executable page)
 * as the scratch code pad, and simply manage it like a stack.  That is,
 * the region below free is 'used'.  It's enough for simple FFI, since
 * the generated code won't live after the dynamic extent of FFI call.
 */
struct ScmCodeCacheRec {
    ScmMemoryRegion *wpad;      /* writable page */
    ScmMemoryRegion *xpad;      /* executable page */
    void *free;
};

#define CODE_PAD_SIZE 4096

static void init_code_cache(ScmVM *vm) {
    if (vm->codeCache != NULL) return;

    ScmCodeCache *cc = SCM_NEW(ScmCodeCache);
    Scm_SysMmapWX(CODE_PAD_SIZE, &cc->wpad, &cc->xpad);
    cc->free = cc->wpad->ptr;
    vm->codeCache = cc;
}

static inline void *allocate_code_cache(ScmVM *vm, size_t size)
{
    ScmCodeCache *cc = vm->codeCache;
    if (cc->free + size > cc->wpad->ptr + cc->wpad->size) {
        Scm_Error("VM code cache overflow");
    }
    void *region = cc->free;
    cc->free += size;
    return region;
}

static inline void free_code_cache(ScmVM *vm, void *ptr)
{
    ScmCodeCache *cc = vm->codeCache;
    SCM_ASSERT(ptr >= cc->wpad->ptr && ptr < cc->wpad->ptr + cc->wpad->size);
    cc->free = ptr;
}

/* Returns address in xpad that corresponds to the wpad_ptr. */
static inline void *get_entry_address(ScmCodeCache *cc, void *wpad_ptr)
{
    return cc->xpad->ptr + (wpad_ptr - cc->wpad->ptr);
}

#if defined(GAUCHE_WINDOWS)
/*
 * Windows x64 exception unwinding requires RUNTIME_FUNCTION + UNWIND_INFO
 * for every non-leaf function in dynamically generated code.  We pack both
 * structs into a single CpPdata block placed right after the codepad code,
 * register it with RtlAddFunctionTable before each call, and deregister it
 * afterward.
 * Cf.  https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64
 *
 * Unwind operation codes used here:
 *   UWOP_ALLOC_SMALL (2): 1-node form, OpInfo = (size/8 - 1), size in 8..128
 *   UWOP_ALLOC_LARGE (1): 2-node form with OpInfo=0, extra slot = size/8
 */
#define CP_UWOP_ALLOC_LARGE  1
#define CP_UWOP_ALLOC_SMALL  2

/*
 * CpPdata layout (20 bytes):
 *   bytes  0-11  RUNTIME_FUNCTION  (BeginAddress, EndAddress, UnwindData)
 *   bytes 12-19  UNWIND_INFO header + two UNWIND_CODE slots
 *
 * The first 12 bytes are layout-compatible with Windows RUNTIME_FUNCTION.
 */
typedef struct {
    /* RUNTIME_FUNCTION (3 x DWORD) */
    uint32_t BeginAddress;
    uint32_t EndAddress;
    uint32_t UnwindData;        /* RVA of VersionFlags field below */
    /* UNWIND_INFO */
    uint8_t  VersionFlags;      /* Version:3=1, Flags:5=0 */
    uint8_t  SizeOfProlog;
    uint8_t  CountOfCodes;
    uint8_t  FrameInfo;         /* FrameRegister=0, FrameOffset=0 */
    /* UnwindCode[0] */
    uint8_t  Code0Offset;
    uint8_t  Code0Op;           /* UnwindOp:4, OpInfo:4 */
    /* UnwindCode[1] — extra data for ALLOC_LARGE, or padding to even count */
    uint16_t Code1;
} CpPdata;

/* Allocation granularity (round to 8 bytes so the next codepad item aligns) */
#define CODEPAD_PDATA_SIZE  ((sizeof(CpPdata) + 7) & ~(size_t)7)

static void setup_codepad_pdata(ScmCodeCache *cc,
                                void *codepad,
                                size_t code_end,   /* EndAddress: past last instruction */
                                size_t pdata_off,  /* where to write CpPdata block */
                                ScmSmallInt entry,
                                ScmSmallInt prolog_end,
                                ScmSmallInt frame_size)
{
    CpPdata *pd = (CpPdata *)((char*)codepad + pdata_off);
    size_t cp_xoff = (size_t)((char*)codepad - (char*)cc->wpad->ptr);
    uint8_t prolog_sz = (uint8_t)(prolog_end - entry);

    SCM_ASSERT(prolog_end > entry && (prolog_end - entry) <= 255);
    SCM_ASSERT(frame_size > 0 && (frame_size % 8) == 0);

    pd->BeginAddress = (uint32_t)(cp_xoff + entry);
    pd->EndAddress   = (uint32_t)(cp_xoff + code_end);
    pd->UnwindData   = (uint32_t)(cp_xoff + pdata_off
                                  + offsetof(CpPdata, VersionFlags));
    pd->VersionFlags  = 1;           /* Version=1, Flags=0 */
    pd->SizeOfProlog  = prolog_sz;
    pd->FrameInfo     = 0;

    if (frame_size <= 128) {
        /* UWOP_ALLOC_SMALL: 1 node, OpInfo = (size/8 - 1) */
        pd->CountOfCodes = 1;
        pd->Code0Offset  = prolog_sz;
        pd->Code0Op      = (uint8_t)(CP_UWOP_ALLOC_SMALL
                                      | (uint8_t)(((frame_size / 8) - 1) << 4));
        pd->Code1        = 0;
    } else {
        /* UWOP_ALLOC_LARGE OpInfo=0: 2 nodes, Code1 = size/8 */
        pd->CountOfCodes = 2;
        pd->Code0Offset  = prolog_sz;
        pd->Code0Op      = (uint8_t)(CP_UWOP_ALLOC_LARGE); /* OpInfo=0 */
        pd->Code1        = (uint16_t)(frame_size / 8);
    }
}
#endif /* GAUCHE_WINDOWS */

/*
 * Copy CODE to the scratch pad, starting from START-th byte up to right before
 * END-th byte, from the pad's TSTART-th position.
 *
 * TEND is, when non-zero, it must be greater than TSTART+(END-START).  The
 * scratch pad area is allocated up to TEND, and filled with zero after the
 * code.
 *
 * Finally, the code is called from the entry offset ENTRY.
 *
 * RETTYPE is  a <native-type> or <top>.
 */

ScmObj Scm__VMCallNative(ScmVM *vm,
                         ScmSmallInt tstart,
                         ScmSmallInt tend,
                         ScmUVector *code,
                         ScmSmallInt start,
                         ScmSmallInt end,
                         ScmSmallInt entry,
                         ScmObj rettype,
                         /* The following two args are used on Windows.
                            On Linux, just pass 0. */
                         ScmSmallInt win_prolog_end SCM_UNUSED,
                         ScmSmallInt win_frame_size SCM_UNUSED)
{
    init_code_cache(vm);

    SCM_ASSERT(SCM_U8VECTORP(code));

    ScmSmallInt uvsize = SCM_UVECTOR_SIZE(code);
    SCM_CHECK_START_END(start, end, uvsize);

    size_t codesize = tstart + end - start;
    if (entry < 0 || (size_t)entry >= codesize) {
        Scm_Error("entry out of range: %ld", entry);
    }

    size_t realcodesize = codesize;
    if (tend > (ScmSmallInt)codesize) realcodesize = tend;

    size_t alloc_size = realcodesize;
#if defined(GAUCHE_WINDOWS)
    /* For Windows, we add PDATA area after the code.  It should start
       at 4-byte alignment. */
    size_t orig_codesize = codesize;  /* before extending for spill slots */
    size_t pdata_off = (realcodesize + 3) & ~(size_t)3;
    if (win_prolog_end > 0 && win_frame_size > 0) {
        alloc_size = pdata_off + CODEPAD_PDATA_SIZE;
    }
#endif

    void *codepad = allocate_code_cache(vm, alloc_size);
    if (tstart > 0) memset(codepad, 0, tstart);
    memcpy(codepad + tstart,
           SCM_UVECTOR_ELEMENTS(code)+start,
           end - start);
    if (realcodesize > codesize) {
        memset(codepad + codesize, 0, realcodesize - codesize);
        codesize = realcodesize;
    }

    ScmObj result = SCM_UNDEFINED;
#if defined(GAUCHE_WINDOWS)
    void * volatile xpad_pdata = NULL;
#endif

    SCM_UNWIND_PROTECT {
        /*
         * On Windows x64, register RUNTIME_FUNCTION + UNWIND_INFO for the
         * codepad so the OS exception unwinder can traverse this frame.
         */
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
        if (win_prolog_end > 0 && win_frame_size > 0) {
            setup_codepad_pdata(vm->codeCache, codepad, orig_codesize,
                                pdata_off,
                                entry, win_prolog_end, win_frame_size);
            xpad_pdata = get_entry_address(vm->codeCache,
                                           (char*)codepad + pdata_off);
            RtlAddFunctionTable((PRUNTIME_FUNCTION)(void*)xpad_pdata, 1,
                                (DWORD64)vm->codeCache->xpad->ptr);
        }
#endif

        /*
         * Call the code
         */
        void *entryPtr = get_entry_address(vm->codeCache, codepad + entry);
        if (SCM_EQ(rettype, Scm_NativeDoubleType())) {
            double r = ((double (*)())entryPtr)();
            result = Scm_VMReturnFlonum(r);
        } else if (SCM_EQ(rettype, Scm_NativeFloatType())) {
            float r = ((float (*)())entryPtr)();
            result = Scm_VMReturnFlonum((double)r);
        } else if (SCM_EQ(rettype, Scm_NativeCStringType())) {
            intptr_t r = ((intptr_t (*)())entryPtr)();
            result = SCM_MAKE_STR_COPYING((const char*)r);
        } else if (SCM_EQ(rettype, Scm_NativeIntptrtType())) {
            intptr_t r = ((intptr_t (*)())entryPtr)();
            result = Scm_IntptrToInteger(r);
        } else if (SCM_EQ(rettype, Scm_NativeUint8Type())) {
            uint8_t r = ((uint8_t (*)())entryPtr)();
            result = SCM_MAKE_INT(r);
        } else if (SCM_C_POINTER_P(rettype) || SCM_C_ARRAY_P(rettype)) {
            void *r = ((void *(*)())entryPtr)();
            result = Scm_MakeNativeHandleSimple(r, rettype);
        } else if (SCM_EQ(rettype, SCM_OBJ(SCM_CLASS_TOP))) {
            intptr_t r = ((intptr_t (*)())entryPtr)();
            result = SCM_OBJ(r);      /* trust the caller */
        } else if (SCM_EQ(rettype, Scm_NativeVoidType())) {
            ((void (*)())entryPtr)();
        } else {
            Scm_Error("unknown return type: %S", rettype);
        }
    } SCM_WHEN_ERROR {
#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
        if (xpad_pdata) {
            RtlDeleteFunctionTable((PRUNTIME_FUNCTION)(void*)xpad_pdata);
        }
#endif
        free_code_cache(vm, codepad);
        SCM_NEXT_HANDLER;
    } SCM_END_PROTECT;

#if defined(GAUCHE_WINDOWS) && defined(__MINGW64__)
    if (xpad_pdata) {
        RtlDeleteFunctionTable((PRUNTIME_FUNCTION)(void*)xpad_pdata);
    }
#endif
    free_code_cache(vm, codepad);
    return result;
}

/*======================================================================
 * JIT support
 */

/* Allocate executable page and copy the machine code in CODE,
   returns an executable <memory-region>.
 */

ScmObj Scm__AllocateCodePage(ScmU8Vector *code)
{
    ScmMemoryRegion *wpad, *xpad;
    long codesize = SCM_U8VECTOR_SIZE(code);
    long pagesize = sys_getpagesize();
    long padsize = ((codesize+pagesize-1)/pagesize)*pagesize;
    Scm_SysMmapWX(padsize, &wpad, &xpad);
    memcpy(wpad->ptr, SCM_U8VECTOR_ELEMENTS(code), codesize);
    /* TODO: If writable page and executable page are mapped into
       two regions, we can unmap writable page here.  Currently we don't
       have an interface to do so explicitly, leaving GC finalizer to
       do the job.
     */
    return SCM_OBJ(xpad);
}


/*======================================================================
 * Initialization
 */

void Scm__InitNative(void)
{
}
