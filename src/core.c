/*
 * core.c - core kernel interface
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/priv/configP.h"
#include "gauche/priv/builtin-syms.h"

/* GC_print_static_roots() is declared in private/gc_priv.h.  It is too much
   hassle to include it with other GC internal baggages, so we just declare
   it.  It's a private function of GC, so watch out the changes in GC. */
extern void GC_print_static_roots(void);

/*
 * out-of-memory handler.  this will be called by GC.
 */

static void *oom_handler(size_t bytes)
{
    Scm_Panic("out of memory (%lu).  aborting...", bytes);
    return NULL;                /* dummy */
}

/*
 * Features list used by cond-expand macro
 */
static struct {
    ScmObj alist;               /* list of (feature) or (feature module) */
    ScmObj dlist;               /* list of disabled features */
    ScmInternalMutex mutex;
} cond_features = { SCM_NIL, SCM_NIL, SCM_INTERNAL_MUTEX_INITIALIZER };

/*=============================================================
 * Program initialization
 */

extern void Scm__InitModule(void);
extern void Scm__InitHash(void);
extern void Scm__InitSymbol(void);
extern void Scm__InitNumber(void);
extern void Scm__InitChar(void);
extern void Scm__InitClass(void);
extern void Scm__InitMemoTable(void);
extern void Scm__InitList(void);
extern void Scm__InitExceptions(void);
extern void Scm__InitPort(void);
extern void Scm__InitWrite(void);
extern void Scm__InitCompaux(void);
extern void Scm__InitMacro(void);
extern void Scm__InitLoad(void);
extern void Scm__InitThreadLocal(void);
extern void Scm__InitParameter(void);
extern void Scm__InitProc(void);
extern void Scm__InitRegexp(void);
extern void Scm__InitRead(void);
extern void Scm__InitSignal(void);
extern void Scm__InitSystem(void);
extern void Scm__InitVM(void);
extern void Scm__InitAutoloads(void);
extern void Scm__InitCollection(void);
extern void Scm__InitComparator(void);
extern void Scm__InitExecenv(void);
extern void Scm__InitLazy(void);
extern void Scm__InitNative(void);
extern void Scm__InitNet(void);
extern void Scm__InitNetAddr(void);
extern void Scm__InitNetDb(void);
extern void Scm__InitMutex(void);
extern void Scm__InitThreads(void);

extern void Scm_Init_libalpha(void);
extern void Scm_Init_libbool(void);
extern void Scm_Init_libbox(void);
extern void Scm_Init_libchar(void);
extern void Scm_Init_libcode(void);
extern void Scm_Init_libcmp(void);
extern void Scm_Init_libdict(void);
extern void Scm_Init_libeval(void);
extern void Scm_Init_libexc(void);
extern void Scm_Init_libfmt(void);
extern void Scm_Init_libhash(void);
extern void Scm_Init_libio(void);
extern void Scm_Init_liblazy(void);
extern void Scm_Init_liblist(void);
extern void Scm_Init_libmemo(void);
extern void Scm_Init_libmisc(void);
extern void Scm_Init_libmod(void);
extern void Scm_Init_libnative(void);
extern void Scm_Init_libnet(void);
extern void Scm_Init_libnum(void);
extern void Scm_Init_libobj(void);
extern void Scm_Init_libproc(void);
extern void Scm_Init_librx(void);
extern void Scm_Init_libsrfis(void);
extern void Scm_Init_libstr(void);
extern void Scm_Init_libsym(void);
extern void Scm_Init_libsys(void);
extern void Scm_Init_libthr(void);
extern void Scm_Init_libtype(void);
extern void Scm_Init_libvec(void);
extern void Scm_Init_libmacbase(void);
extern void Scm_Init_compile(void);
extern void Scm_Init_libmacro(void);
extern void Scm_Init_libparam(void);
extern void Scm_Init_libomega(void);

extern void Scm__FinishModuleInitialization(void);

static void finalizable(void);
static void init_cond_features(void);

#ifdef GAUCHE_USE_PTHREADS
/* a trick to make sure the gc thread object is linked */
static int (*ptr_pthread_create)(pthread_t *,
                                 GC_PTHREAD_CREATE_CONST pthread_attr_t *,
                                 void *(*)(void *), void * /* arg */) = NULL;
#endif

/* To track how far we've gone through booting stages.  See gauche.h
   This variable is changed before any other threads are created, so no mutex
   is needed. */
static ScmRuntimeState runtime_state = SCM_RUNTIME_INITIALIZING;

/*
 * Entry point of initlalizing Gauche runtime
 */
void Scm_Init(const char *signature)
{
    if (runtime_state > SCM_RUNTIME_INITIALIZING) return;

    int debug_init = FALSE;
    if (Scm_GetEnv("GAUCHE_DEBUG_INITIALIZATION") != NULL) {
        debug_init = TRUE;
    }
#define CALL_INIT(f)                                            \
    do {                                                        \
        if (debug_init) fprintf(stderr, "Calling %s...\n", #f); \
        f();                                                    \
    } while (0)

    /* make sure the main program links the same version of libgauche */
    if (strcmp(signature, GAUCHE_SIGNATURE) != 0) {
        Scm_Panic("libgauche ABI version mismatch: libgauche %s, expected %s",
                  GAUCHE_SIGNATURE, signature);
    }

    /* Some platforms require this.  It is harmless if GC is
       already initialized, so we call it here just in case. */
    GC_init();

    /* Set up GC parameters.  We need to call finalizers at the safe
       point of VM loop, so we disable auto finalizer invocation, and
       ask GC to call us back when finalizers are queued. */
    GC_set_oom_fn(oom_handler);
    GC_set_finalize_on_demand(TRUE);
    GC_set_finalizer_notifier(finalizable);

    /* Newer bdwgc delays spawning marker threads until the client creates
       first thread.  We can take advantage of parallel markers even with
       single-threaded program, so we ask them to go parallel now.  */
    GC_start_mark_threads();

    (void)SCM_INTERNAL_MUTEX_INIT(cond_features.mutex);

    /* Initialize components.  The order is important, for some components
       rely on the other components to be initialized. */
    CALL_INIT(Scm__InitThreadLocal);
    CALL_INIT(Scm__InitParameter);
    CALL_INIT(Scm__InitVM);
    CALL_INIT(Scm__InitHash);
    CALL_INIT(Scm__InitSymbol);
    CALL_INIT(Scm__InitModule);
    CALL_INIT(Scm__InitNumber);
    CALL_INIT(Scm__InitChar);
    CALL_INIT(Scm__InitClass);
    CALL_INIT(Scm__InitMemoTable);
    CALL_INIT(Scm__InitList);
    CALL_INIT(Scm__InitCollection);
    CALL_INIT(Scm__InitExceptions);
    CALL_INIT(Scm__InitProc);
    CALL_INIT(Scm__InitPort);
    CALL_INIT(Scm__InitWrite);
    CALL_INIT(Scm__InitMacro);
    CALL_INIT(Scm__InitLoad);
    CALL_INIT(Scm__InitRegexp);
    CALL_INIT(Scm__InitRead);
    CALL_INIT(Scm__InitSignal);
    CALL_INIT(Scm__InitSystem);
    CALL_INIT(Scm__InitComparator);
    CALL_INIT(Scm__InitExecenv);
    CALL_INIT(Scm__InitNative);
    CALL_INIT(Scm__InitNet);
    CALL_INIT(Scm__InitNetAddr);
    CALL_INIT(Scm__InitNetDb);
    CALL_INIT(Scm__InitMutex);
    CALL_INIT(Scm__InitThreads);

    CALL_INIT(Scm_Init_libalpha);
    CALL_INIT(Scm_Init_libbool);
    CALL_INIT(Scm_Init_libbox);
    CALL_INIT(Scm_Init_libchar);
    CALL_INIT(Scm_Init_libcode);
    CALL_INIT(Scm_Init_libcmp);
    CALL_INIT(Scm_Init_libeval);
    CALL_INIT(Scm_Init_libexc);
    CALL_INIT(Scm_Init_libfmt);
    CALL_INIT(Scm_Init_libhash);
    CALL_INIT(Scm_Init_libio);
    CALL_INIT(Scm_Init_liblazy);
    CALL_INIT(Scm_Init_liblist);
    CALL_INIT(Scm_Init_libmemo);
    CALL_INIT(Scm_Init_libmisc);
    CALL_INIT(Scm_Init_libmod);
    CALL_INIT(Scm_Init_libnative);
    CALL_INIT(Scm_Init_libnet);
    CALL_INIT(Scm_Init_libnum);
    CALL_INIT(Scm_Init_libobj);
    CALL_INIT(Scm_Init_libproc);
    CALL_INIT(Scm_Init_librx);
    CALL_INIT(Scm_Init_libsrfis);
    CALL_INIT(Scm_Init_libstr);
    CALL_INIT(Scm_Init_libsym);
    CALL_INIT(Scm_Init_libsys);
    CALL_INIT(Scm_Init_libvec);
    CALL_INIT(Scm_Init_libmacbase);
    CALL_INIT(Scm_Init_compile);
    /* lib* that uses er-macro-transformer must be initialized
       after libmacro */
    CALL_INIT(Scm_Init_libmacro);
    CALL_INIT(Scm_Init_libparam);
    CALL_INIT(Scm_Init_libtype);
    CALL_INIT(Scm_Init_libthr);
    CALL_INIT(Scm_Init_libdict);
    CALL_INIT(Scm_Init_libomega);

    CALL_INIT(Scm__InitCompaux);

    Scm_SelectModule(Scm_GaucheModule());
    CALL_INIT(Scm__InitAutoloads);

    Scm_SelectModule(Scm_UserModule());

    /* Final setup of cond-features alist. */
    CALL_INIT(init_cond_features);

#ifdef GAUCHE_USE_PTHREADS
    /* a trick to make sure the gc thread object is linked */
    ptr_pthread_create = GC_pthread_create;
#endif

    Scm__FinishModuleInitialization();
    runtime_state = SCM_RUNTIME_INITIALIZED;
}

/*
 * Runtime state
 */

ScmRuntimeState Scm_RuntimeState()
{
    return runtime_state;
}

/* This should only be called from main.c or equivalent.  */
void Scm_SetRuntimeReplState(int full)
{
    runtime_state = full? SCM_RUNTIME_FULL_REPL : SCM_RUNTIME_MINI_REPL;
}

/*=============================================================
 * Memory utilities
 */

#if SCM_ATOMIC_NEED_HELPER
/* This is only needed if we fall back to libatomic_ops and not GNUC */
#include "gauche/priv/atomicP.h"

int Scm__AtomicCompareExchange(ScmAtomicVar *loc,
                               ScmAtomicWord *expectedloc,
                               ScmAtomicWord newval)
{
    ScmAtomicWord expected = *expectedloc;
    ScmAtomicWord oldval = AO_fetch_compare_and_swap_full(loc,
                                                          expected,
                                                          newval);
    *expectedloc = expected;
    return oldval == expected;
}

ScmAtomicWord Scm__AtomicExchange(ScmAtomicVar *loc,
                                  ScmAtomicWord newval)
{
    ScmAtomicWord current;
    do {
        current = *loc;
    } while (AO_compare_and_swap_full(loc, current, newval));
    return current;
}
#endif /*SCM_ATOMIC_NEED_HELPER*/


/*=============================================================
 * GC utilities
 */

void Scm_GC()
{
    GC_gcollect();
}

void Scm_PrintStaticRoots()
{
    GC_print_static_roots();
}

/*
 * External API to register root set in dynamically loaded library.
 * Boehm GC doesn't do this automatically on some platforms.
 *
 * NB: The scheme we're using to find bss area (by Scm__bss{start|end})
 * is getting less effective, since more platforms are adopting the
 * linker that rearranges bss variables.  The extensions should not
 * keep GC_MALLOCED pointer into the bss variable.
 */
void Scm_RegisterDL(void *data_start, void *data_end,
                    void *bss_start, void *bss_end)
{
    if (data_start < data_end) {
        GC_add_roots(data_start, data_end);
    }
    if (bss_start < bss_end) {
        GC_add_roots(bss_start, bss_end);
    }
}

/*
 * Useful routine for debugging, to check if an object is inadvertently
 * collected.
 */
static void gc_sentinel(ScmObj obj, void *data)
{
    Scm_Printf(SCM_CURERR, "WARNING: object %s(%p) is inadvertently collected\n", (char *)data, obj);
}

void Scm_GCSentinel(void *obj, const char *name)
{
    Scm_RegisterFinalizer(SCM_OBJ(obj), gc_sentinel, (void*)name);
}


/*=============================================================
 * Finalization.  Scheme finalizers are added as NO_ORDER.
 */
void Scm_RegisterFinalizer(ScmObj z, ScmFinalizerProc finalizer, void *data)
{
    GC_finalization_proc ofn; void *ocd;
    GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)finalizer,
                                   data, &ofn, &ocd);
}

void Scm_UnregisterFinalizer(ScmObj z)
{
    GC_finalization_proc ofn; void *ocd;
    GC_REGISTER_FINALIZER_NO_ORDER(z, (GC_finalization_proc)NULL, NULL,
                                   &ofn, &ocd);
}

/* GC calls this back when finalizers are queued. */
void finalizable(void)
{
    ScmVM *vm = Scm_VM();
    if (vm != NULL) {
        vm->finalizerPending = TRUE;
        vm->attentionRequest = TRUE;
    }
}

/* Called from VM loop.  Queue is not empty. */
ScmObj Scm_VMFinalizerRun(ScmVM *vm)
{
    GC_invoke_finalizers();
    vm->finalizerPending = FALSE;
    return SCM_UNDEFINED;
}

/*=============================================================
 * Program cleanup & termination
 */

struct cleanup_handler_rec {
    void (*handler)(void *data);
    void *data;
    struct cleanup_handler_rec *next;
};

static struct {
    int dirty;                  /* Flag to avoid cleaning up more than once. */
    struct cleanup_handler_rec *handlers;
} cleanup = { TRUE, NULL };

/* Add cleanup handler.  Returns an opaque handle, which can be
   passed to DeleteCleanupHandler. */
void *Scm_AddCleanupHandler(void (*h)(void *d), void *d)
{
    struct cleanup_handler_rec *r = SCM_NEW(struct cleanup_handler_rec);
    r->handler = h;
    r->data = d;
    r->next = cleanup.handlers;
    cleanup.handlers = r;
    return r;
}

/* Delete cleanup handler.  HANDLE should be an opaque pointer
   returned from Scm_AddCleanupHandler, but it won't complain if
   other pointer is given. */
void Scm_DeleteCleanupHandler(void *handle)
{
    struct cleanup_handler_rec *x = NULL, *y = cleanup.handlers;
    while (y) {
        if (y == handle) {
            if (x == NULL) {
                cleanup.handlers = y->next;
            } else {
                x->next = y->next;
            }
            break;
        }
    }
}

void Scm__MutexCleanup(void *mutex_)
{
    ScmInternalMutex *mutex = mutex_;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(*mutex);
}


/* Scm_Cleanup and Scm_Exit
   Usually calling Scm_Exit is the easiest way to terminate Gauche
   application safely.  If the application wants to continue operation
   after shutting down the Scheme part, however, it can call Scm_Cleanup().
*/

/* To avoid complication in supporting different platforms */
#define EXIT_CODE(code) ((code)&0xff)

void Scm_Exit(int code)
{
    Scm_Cleanup();
    exit(EXIT_CODE(code));
}

void Scm_Cleanup(void)
{
    if (!cleanup.dirty) return;
    cleanup.dirty = FALSE;

    Scm_VMFlushDynamicHandlers();

    /* Call the C-registered cleanup handlers. */
    for (struct cleanup_handler_rec *ch = cleanup.handlers; ch; ch = ch->next) {
        ch->handler(ch->data);
    }

    /* Flush Scheme ports. */
    Scm_FlushAllPorts(TRUE);
}

int Scm_ObjToExitCode(ScmObj obj)
{
    if (SCM_EQ(obj, SCM_TRUE)) {
        return 0;
    } else if (SCM_EQ(obj, SCM_FALSE)) {
        return 1;
    } else if (SCM_INTP(obj)) {
        return SCM_INT_VALUE(obj) & 0xff;
    } else if (SCM_INTEGERP(obj)) {
        return Scm_GetInteger(Scm_LogAnd(obj, SCM_MAKE_INT(0xff)));
    } else {
        return 70;              /* EX_SOFTWARE */
    }
}

void Scm_Panic(const char *msg, ...)
{
    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);
    fputc('\n', stderr);
    fflush(stderr);
    _exit(EXIT_CODE(1));
}

/* Use this for absolute emergency.  Newline is not attached to msg. */
void Scm_Abort(const char *msg)
{
    int size = (int)strlen(msg);
    /* this may return an error, but we don't care, since we exit anyway. */
    SCM_IGNORE_RESULT(write(2, msg, size));
    _exit(EXIT_CODE(1));
}

/*=============================================================
 * Inspect the configuration
 */

/* Returns the cond-features alist. */
ScmObj
Scm_GetFeatures()
{
    return cond_features.alist;
}

/*
 * Enables FEATURE.  Once added, the symbol FEATURE will be recognized
 * by cond-expand.
 *
 *  (cond-expand (FEATURE body ...))
 *
 * If loading a module is required in order to make FEATURE available,
 * such module name can be specified in MODULE argument.  It can be NULL
 * if the feature is built-in.
 *
 * If FEATURE is already in the disabled list, it is not added.
 *
 * This API is meant to expose configuration information to a Scheme
 * level.
 */
void
Scm_AddFeature(const char *feature, const char *module)
{
    ScmObj f = SCM_INTERN(feature);
    ScmObj cell = module? SCM_LIST2(f, SCM_INTERN(module)): SCM_LIST1(f);

    (void)SCM_INTERNAL_MUTEX_LOCK(cond_features.mutex);
    if (SCM_FALSEP(Scm_Memq(f, cond_features.dlist))) {
        cond_features.alist = Scm_Cons(cell, cond_features.alist);
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(cond_features.mutex);
}

/*
 * Disables FEATURE.  If FEATURE is already enabled, it is removed
 * from the features alist.  It also prevents FEATURE to be added
 * in the future.
 *
 * Disabling takes precedence of AddFeature; there's no way to make
 * the FEATURE available once it is disabled.
 *
 * Mainly used by gosh's -F-feature command-line option.
 */
void
Scm_DisableFeature(const char *feature)
{
    ScmObj f = SCM_INTERN(feature);
    (void)SCM_INTERNAL_MUTEX_LOCK(cond_features.mutex);
    cond_features.dlist = Scm_Cons(f, cond_features.dlist);
    cond_features.alist = Scm_AssocDelete(f, cond_features.alist, SCM_CMP_EQ);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(cond_features.mutex);
}

/*
 * Remove FEATURE from the feature list.
 */
void
Scm_DeleteFeature(const char *feature)
{
    ScmObj f = SCM_INTERN(feature);
    (void)SCM_INTERNAL_MUTEX_LOCK(cond_features.mutex);
    cond_features.alist = Scm_AssocDelete(f, cond_features.alist, SCM_CMP_EQ);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(cond_features.mutex);
}


static void
init_cond_features()
{
    /* The initial cond-features list. */
    static struct {
        const char *feature;
        const char *module;
    } init_features[] = {
        { "gauche", NULL },
        { "gauche-" GAUCHE_VERSION , NULL },

        /* Platform */
#if   defined(GAUCHE_WINDOWS)
        { "gauche.os.windows", NULL },
        { "gauche-windows", NULL }, /* for backward compatibility */
#elif defined(__CYGWIN__)  /* cygwin is different enough to deserve this */
        { "gauche.os.cygwin", NULL },
#endif

        /* R7RS */
        /* NB: We should probably make checking 'r7rs' trigger loading
           r7rs module and setting up the environment. */
        { "r7rs", NULL },

        /* R7RS Appendix B */
        { "exact-closed", NULL },
        // { "exact-complex", NULL }, /* not yet */
        { "ieee-float", NULL },
        { "full-unicode", NULL },
        { "ratios", NULL },
#if   defined(GAUCHE_WINDOWS)
        { "windows", NULL },
#else
        { "posix", NULL },
#endif
        /* OS/CPU arch flags are generated to features.c by gen-features.sh */
        /* TODO: C memory model flags */
#if   defined(WORDS_BIGENDIAN)
        { "big-endian", NULL },
#else
        { "little-endian", NULL }, /* NB: r7rs say nothing on mixed endian */
#endif

        /* SRFIs that are not libraries */
        { "srfi-22", NULL },    /* Scheme scripts */
        { "srfi-138", NULL },    /* compile-r7rs script */

        /* Threads */
#if   defined(GAUCHE_USE_PTHREADS)
        { "gauche.sys.threads", "gauche.threads" },
        { "gauche.sys.pthreads", "gauche.threads" },
#elif defined(GAUCHE_USE_WTHREADS)
        { "gauche.sys.threads", "gauche.threads" },
        { "gauche.sys.wthreads", "gauche.threads" },
#endif

        /* TLS/SSL.  This nees to be in the core in order to switch
           code _before_ loading rfc.tls */
#if defined(GAUCHE_USE_MBEDTLS)
        { "gauche.net.tls", "rfc.tls" },
#endif
#if defined(GAUCHE_USE_MBEDTLS)
        /* NB: Kludge - mbedTLS is implemented in a separate module rfc.tls.mbed,
           but we don't autoload it with cond-expand gauche.net.tls.mbedtls.
           If the gauche is compiled with mbedTLS support but the target system
           lacks mbed DSO, using rfc.tls.mbed causes runtime error.  We don't
           want that merely by checking feature identifier.  We set autoload
           in rfc.tls, so the runtime error only occur when the program actually
           try to use <mbed-tls>. */
        { "gauche.net.tls.mbedtls", NULL },
#endif

        /* zlib */
#if defined(USE_ZLIB)
        { "gauche.sys.zlib", "rfc.zlib" },
#endif

        { "regexp-non-greedy", "srfi.115" },
        { "regexp-look-around", "srfi.115" },
        { "regexp-backrefs", "srfi.115" },
        { "regexp-unicode", "srfi.115" },

#include "features.c"

        { NULL, NULL }
    };

    for (int i=0; init_features[i].feature; i++) {
        Scm_AddFeature(init_features[i].feature, init_features[i].module);
    }
}

#include "buildinfo.c"

ScmObj Scm_BuildGoshVersion()
{
    return SCM_MAKE_STR(build_gosh_version);
}


/*=============================================================
 * 'Main'
 */

/*
 * When creating DLL under Cygwin, we need the following dummy main()
 * or we get "undefined reference _WinMain@16" error.
 * (See cygwin FAQ, http://cygwin.com/faq/)
 */
#ifdef __CYGWIN__
int main(void)
{
    return 0;
}
#endif /*__CYGWIN__*/

/*
 * A simple main routine useful to build a binary executable.
 */

void Scm_SimpleMain(int argc, const char *argv[],
                    const char *script, u_long flags SCM_UNUSED)
{
    SCM_ASSERT(argc > 0);
    ScmObj args = Scm_InitCommandLine2(argc, argv, SCM_COMMAND_LINE_BOTH);

    if (script) {
        ScmObj s = SCM_MAKE_STR(script);
        ScmObj p = Scm_MakeInputStringPort(SCM_STRING(s), TRUE);
        Scm_LoadFromPort(SCM_PORT(p), SCM_LOAD_PROPAGATE_ERROR, NULL);
    }

    ScmModule *user = Scm_UserModule();
    ScmObj mainproc = Scm_GlobalVariableRef(user, SCM_SYMBOL(SCM_INTERN("main")), 0);
    if (SCM_PROCEDUREP(mainproc)) {
        static ScmObj run_main_proc = SCM_UNDEFINED;
        SCM_BIND_PROC(run_main_proc, "run-main", Scm_GaucheInternalModule());
        SCM_ASSERT(SCM_PROCEDUREP(run_main_proc));

        ScmEvalPacket epak;
        int r = Scm_Apply(run_main_proc, SCM_LIST2(mainproc, args), &epak);
        if (r >= 0 && SCM_INTP(epak.results[0])) {
            Scm_Exit(SCM_INT_VALUE(epak.results[0]));
        } else {
            Scm_Exit(70);       /* EX_SOFTWARE */
        }
    } else {
        Scm_Exit(0);
    }
}
