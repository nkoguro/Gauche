/*
 * charconv.c - character code conversion library
 *
 *  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: charconv.c,v 1.35 2002-09-26 05:24:44 shirok Exp $
 */

#include <string.h>
#include <errno.h>
#include <gauche.h>
#include <gauche/extend.h>
#include "charconv.h"

#define DEFAULT_CONVERSION_BUFFER_SIZE 1024

typedef struct conv_guess_rec {
    const char *codeName;
    ScmCodeGuessingProc proc;
    void *data;
    struct conv_guess_rec *next;
} conv_guess;

/* anchor of the chain of conversion guessing procedures */
static struct {
    conv_guess *procs;
    ScmInternalMutex mutex;
} guess = { NULL };

/* anchor of the conversion context used for UCS -> internal char routine */
static struct {
    ScmConvInfo *ucs2char;
    ScmConvInfo *char2ucs;
    ScmInternalMutex mutex;
} ucsconv = { NULL };

/*------------------------------------------------------------
 * Query
 */

/* Auxiliary function */
const char* Scm_GetCESName(ScmObj code, const char *argname)
{
    const char *c = NULL;
    if (SCM_UNBOUNDP(code) || SCM_FALSEP(code)) {
        c = Scm_SupportedCharacterEncodings()[0];
    } else if (!SCM_STRINGP(code)) {
        Scm_Error("string or #f is required for %s, but got %S",
                  argname, code);
    } else {
        c = Scm_GetStringConst(SCM_STRING(code));
    }
    return c;
}

int Scm_ConversionSupportedP(const char *from, const char *to)
{
    ScmConvInfo *info = jconv_open(to, from);
    if (info == NULL) return FALSE;
    jconv_close(info);
    return TRUE;
}

void Scm_RegisterCodeGuessingProc(const char *code,
                                  ScmCodeGuessingProc proc,
                                  void *data)
{
    conv_guess *rec = SCM_NEW(conv_guess);
    rec->codeName = code;
    rec->proc = proc;
    rec->data = data;
    (void)SCM_INTERNAL_MUTEX_LOCK(guess.mutex);
    rec->next = guess.procs;
    guess.procs = rec;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(guess.mutex);
}

static conv_guess *findGuessingProc(const char *code)
{
    conv_guess *rec;
    (void)SCM_INTERNAL_MUTEX_LOCK(guess.mutex);
    for (rec = guess.procs; rec; rec = rec->next) {
        if (strcasecmp(rec->codeName, code) == 0) break;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(guess.mutex);
    return rec;
}

static int conv_fileno(ScmPort *port)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    return Scm_PortFileNo(info->remote);
}

static int conv_ready(ScmPort *port)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    /* This isn't accurate, but for now ... */
    return Scm_CharReady(info->remote);
}

static ScmObj conv_name(int dir, ScmPort *remote, const char *from, const char *to)
{
    ScmObj out = Scm_MakeOutputStringPort();
    Scm_Printf(SCM_PORT(out), "[conv(%s->%s) %s %S]",
               from, to, (dir == SCM_PORT_INPUT? "from" : "to"),
               Scm_PortName(remote));
    return Scm_GetOutputString(SCM_PORT(out));
}

/*------------------------------------------------------------
 * Input conversion
 *
 *  <-- Bufferd port <--- filler <--(info->buf)--- getz(remote)
 */

static int conv_input_filler(ScmPort *port, int mincnt)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    size_t insize, inroom, outroom, result;
    int nread;
    const char *inbuf = info->buf;
    char *outbuf = port->src.buf.end;

    /* Fill the input buffer.  There may be some remaining bytes in the
       inbuf from the last conversion (insize), so we try to fill the
       rest. */
    insize = info->ptr - info->buf;
    nread = Scm_Getz(info->ptr, info->bufsiz - insize, info->remote);
    if (nread <= 0) {
        /* input reached EOF.  finish the output state */
        if (insize == 0) {
            outroom = SCM_PORT_BUFFER_ROOM(port);
            result = jconv_reset(info, outbuf, outroom);
            if (result < 0) {
                /* The port buffer doesn't have enough space to contain the
                   finishing sequence.  Its unusual, for the port buffer
                   must be almost empty at this time, and the finishing
                   sequence is usually just a few bytes.
                   We signal an error. */
                Scm_Error("couldn't flush the ending escape sequence in the character encoding conversion port (%s -> %s).  possibly an implementation error",
                          info->fromCode, info->toCode);
            }
            if (info->ownerp) Scm_ClosePort(info->remote);
#ifdef JCONV_DEBUG
            fprintf(stderr, "<= r=%d (reset), out(%p)%d\n",
                    result, outbuf, outroom);
#endif
            return result;
        }
    } else {
        insize += nread;
    }

    /* Conversion. */
    inroom = insize;
    outroom = SCM_PORT_BUFFER_ROOM(port);

#ifdef JCONV_DEBUG
    fprintf(stderr, "=> in(%p)%d out(%p)%d\n", inbuf, insize, outbuf, outroom);
#endif
    result = jconv(info, &inbuf, &inroom, &outbuf, &outroom);
#ifdef JCONV_DEBUG
    fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
            result, inbuf, inroom, outbuf, outroom);
#endif
    /* we've got an error. */
    if (result == INPUT_NOT_ENOUGH || result == OUTPUT_NOT_ENOUGH) {
        /* Conversion stopped due to an incomplete character at the
           end of the input buffer, or the output buffer is full.
           We shift the unconverted bytes to the beginning of input
           buffer. */
        memmove(info->buf, info->buf+insize-inroom, inroom);
        info->ptr = info->buf + inroom;
        return info->bufsiz - outroom;
    } else if (result == ILLEGAL_SEQUENCE) {
        /* it's likely that the input contains invalid sequence. */
        int cnt = inroom >= 6 ? 6 : inroom;
        ScmObj s = Scm_MakeString(info->buf+insize-inroom, cnt, cnt,
                                  SCM_MAKSTR_COPYING|SCM_MAKSTR_INCOMPLETE);
        Scm_Error("invalid character sequence in the input stream: %S ...", s);
    }

    /* Conversion is done completely. */
    /* NB: There are cases that some bytes are left in the input buffer
       even iconv returns positive value.  We need to shift those bytes. */
    if (inroom > 0) {
        memmove(info->buf, info->buf+insize-inroom, inroom);
        info->ptr = info->buf + inroom;
        return info->bufsiz - outroom;
    } else {
        info->ptr = info->buf;
        return info->bufsiz - outroom;
    }
}

static int conv_input_closer(ScmPort *p)
{
    ScmConvInfo *info = (ScmConvInfo*)p->src.buf.data;
    return jconv_close(info);
}

ScmObj Scm_MakeInputConversionPort(ScmPort *fromPort,
                                   const char *fromCode,
                                   const char *toCode,
                                   ScmObj handler,
                                   int bufsiz,
                                   int ownerp)
{
    ScmConvInfo *cinfo;
    conv_guess *guess;
    char *inbuf = NULL;
    int preread = 0;
    ScmPortBuffer bufrec;
    ScmObj name;

    if (!SCM_IPORTP(fromPort))
        Scm_Error("input port required, but got %S", fromPort);

    if (bufsiz <= 0) bufsiz = DEFAULT_CONVERSION_BUFFER_SIZE;
    guess = findGuessingProc(fromCode);
    if (guess) {
        const char *guessed;
        
        inbuf = SCM_NEW_ATOMIC2(char *, bufsiz);
        preread = Scm_Getz(inbuf, bufsiz, fromPort);
        if (preread <= 0) {
            /* Input buffer is already empty or unreadable.
               Determining character code is not necessary.
               We just return a dummy empty port. */
            return Scm_MakeInputStringPort(SCM_STRING(SCM_MAKE_STR("")));
        }
        guessed = guess->proc(inbuf, preread, guess->data);
        if (guessed == NULL)
            Scm_Error("%s: failed to guess input encoding", fromCode);
        fromCode = guessed;
    }

    cinfo = jconv_open(toCode, fromCode);
    if (cinfo == NULL) {
        Scm_Error("conversion from code %s to code %s is not supported",
                  fromCode, toCode);
    }
    cinfo->remote = fromPort;
    cinfo->ownerp = ownerp;
    cinfo->bufsiz = bufsiz;
    if (preread > 0) {
        cinfo->buf = inbuf;
        cinfo->ptr = inbuf + preread;
    } else {
        cinfo->buf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
        cinfo->ptr = cinfo->buf;
    }

    bufrec.size = cinfo->bufsiz;
    bufrec.buffer = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    bufrec.mode = SCM_PORT_BUFFER_FULL;
    bufrec.filler = conv_input_filler;
    bufrec.flusher = NULL;
    bufrec.closer = conv_input_closer;
    bufrec.ready = conv_ready;
    bufrec.filenum = conv_fileno;
    bufrec.data = (void*)cinfo;

    name = conv_name(SCM_PORT_INPUT, fromPort, fromCode, toCode);
    return Scm_MakeBufferedPort(name, SCM_PORT_INPUT, TRUE, &bufrec);
}

/*------------------------------------------------------------
 * Output conversion
 *
 *   Bufferd port ----> flusher -->(info->buf)--> putz(remote)
 */

/* NB: Glibc-2.1.2's iconv() has a bug in SJIS handling.  If output
 * is in SJIS and output buffer overflows in the middle of two-byte
 * sequence, it leaves the first byte in the output buffer as if
 * it were valid converted character, while the input buffer pointer
 * stops just before the unconverted character, as supposed.
 * There's no way to detect that unless I scan the output by myself
 * to see the last byte of conversion is invalid or not.
 *
 * As a workaround, I flush the output buffer more frequently than
 * needed, avoiding the situation that the output buffer overflow.
 * Hoping the bugs are fixed in the future release of glibc.
 */

#define GLIBC_2_1_ICONV_BUG

static int conv_output_closer(ScmPort *port)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    int r;
    
    /* if there's remaining bytes in buf, send them to the remote port. */
    if (info->ptr > info->buf) {
        Scm_Putz(info->buf, info->ptr - info->buf, info->remote);
        info->ptr = info->buf;
    }
    /* sends out the closing sequence, if any */
    r = jconv_reset(info, info->buf, info->bufsiz);
    if (r < 0) {
        Scm_Error("something wrong in resetting output character encoding conversion (%s -> %s).  possibly implementation error.",
                  info->fromCode, info->toCode);
    }
    if (r > 0) {
        Scm_Putz(info->buf, r, info->remote);
    }
    /* flush remove port */
    Scm_Flush(info->remote);
    if (info->ownerp) Scm_ClosePort(info->remote);
    return jconv_close(info);
}

static int conv_output_flusher(ScmPort *port, int mincnt)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    size_t outsize, inroom, outroom, result, len;
    const char *inbuf;
    char *outbuf;

    inbuf = port->src.buf.buffer;
    inroom = len = SCM_PORT_BUFFER_AVAIL(port);
    for (;;) {
        /* Conversion. */
        outbuf = info->ptr;
        outsize = info->bufsiz - (info->ptr - info->buf);
        outroom = outsize;
#ifdef JCONV_DEBUG
        fprintf(stderr, "=> in(%p,%p)%d out(%p,%p)%d\n",
                inbuf, len, inroom,
                info->buf, info->ptr, outroom);
#endif
        result = jconv(info, &inbuf, &inroom, &outbuf, &outroom);
#ifdef JCONV_DEBUG
        fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
                result, inbuf, inroom, outbuf, outroom);
#endif
        if (result == INPUT_NOT_ENOUGH) {
#ifndef GLIBC_2_1_ICONV_BUG
            /* Conversion stopped due to an incomplete character at the
               end of the input buffer.  We just return # of bytes
               flushed.  (Shifting unconverted characters is done by
               buffered port routine) */
            info->ptr = outbuf;
            return len - inroom;
#else
            /* See the above notes.  We always flush the output buffer
               here, so that we can avoid output buffer overrun. */
            Scm_Putz(info->buf, outbuf - info->buf, info->remote);
            info->ptr = info->buf;
            return len - inroom;
#endif
        } else if (result == OUTPUT_NOT_ENOUGH) {
            /* Output buffer got full.  Flush it, and continue
               conversion. */
            Scm_Putz(info->buf, outbuf - info->buf, info->remote);
            info->ptr = info->buf;
            continue;
        } else if (result == ILLEGAL_SEQUENCE) {
            /* it's likely that input contains invalid sequence.
               TODO: we should handle this case gracefully. */
            Scm_Error("invalid character sequence in the input stream");
            return 0;           /* dummy */
        } else {
#ifndef GLIBC_2_1_ICONV_BUG
            /* Conversion is done completely.  Update outptr. */
            info->ptr = outbuf;
            return len - inroom;
#else
            /* See the above notes.  We always flush the output buffer here,
               so that we can avoid output buffer overrun. */
            Scm_Putz(info->buf, outbuf - info->buf, info->remote);
            info->ptr = info->buf;
            return len - inroom;
#endif
        }
    }
}

ScmObj Scm_MakeOutputConversionPort(ScmPort *toPort,
                                    const char *toCode,
                                    const char *fromCode,
                                    int bufsiz, int ownerp)
{
    ScmConvInfo *cinfo;
    ScmPortBuffer bufrec;
    ScmObj name;
    
    if (!SCM_OPORTP(toPort))
        Scm_Error("output port required, but got %S", toPort);
    
    cinfo = jconv_open(toCode, fromCode);
    if (cinfo == NULL) {
        Scm_Error("conversion from code %s to code %s is not supported",
                  fromCode, toCode);
    }
    cinfo->remote = toPort;
    cinfo->ownerp = ownerp;
    cinfo->bufsiz = (bufsiz > 0)? bufsiz : DEFAULT_CONVERSION_BUFFER_SIZE;
    cinfo->buf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->ptr = cinfo->buf;
    
    bufrec.size = cinfo->bufsiz;
    bufrec.buffer = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    bufrec.mode = SCM_PORT_BUFFER_FULL;
    bufrec.filler = NULL;
    bufrec.flusher = conv_output_flusher;
    bufrec.closer = conv_output_closer;
    bufrec.ready = conv_ready;
    bufrec.filenum = conv_fileno;
    bufrec.data = (void*)cinfo;
    
    name = conv_name(SCM_PORT_OUTPUT, toPort, fromCode, toCode);
    return Scm_MakeBufferedPort(name, SCM_PORT_OUTPUT, TRUE, &bufrec);
}

/*------------------------------------------------------------
 * Direct interface for code guessing
 */
const char *Scm_GuessCES(const char *code, const char *buf, int buflen)
{
    conv_guess *guess = findGuessingProc(code);
    if (guess == NULL)
        Scm_Error("unknown code guessing scheme: %s", code);
    return guess->proc(buf, buflen, guess->data);
}

/*------------------------------------------------------------
 * UCS4 <-> internal character routine
 *
 * These routines are called when the literal character is given by
 * unicode notation (#\uXXXx, #\UXXXXXXXX or \uXXXX, \UXXXXXXXX inside 
 * string), or unicode->char routine is called.
 * For this purpose, we keep two global conversion context.
 * Since internal encodings are stateless, we can reuse those
 * context, instead of calling jconv_open every time.
 */

static ScmChar ucstochar(int ucs4)
{
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return (ScmChar)ucs4;
#else  /*!GAUCHE_CHAR_ENCODING_UTF_8*/
    char inbuf[6], outbuf[6];
    const char *inb = inbuf;
    char *outb = outbuf;
    size_t inroom, outroom, r;
    
    if (ucsconv.ucs2char == NULL) return SCM_CHAR_INVALID;
    inroom = UCS2UTF_NBYTES(ucs4);
    outroom = 6;
    jconv_ucs4_to_utf8(ucs4, inbuf);
    (void)SCM_INTERNAL_MUTEX_LOCK(ucsconv.mutex);
    r = jconv(ucsconv.ucs2char, &inb, &inroom, &outb, &outroom);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ucsconv.mutex);
    if (r == INPUT_NOT_ENOUGH || r == OUTPUT_NOT_ENOUGH) {
        Scm_Error("can't convert UCS4 code %d to a character: implementation problem?", ucs4);
    } else if (r == ILLEGAL_SEQUENCE) {
        return SCM_CHAR_INVALID;
    } else {
        ScmChar out;
        SCM_CHAR_GET(outbuf, out);
        return out;
    }
#endif /*!GAUCHE_CHAR_ENCODING_UTF_8*/
}

static int chartoucs(ScmChar ch)
{
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    if (ch == SCM_CHAR_INVALID) return -1;
    return (int)ch;
#else  /*!GAUCHE_CHAR_ENCODING_UTF_8*/
    char inbuf[6], outbuf[6];
    const char *inb = inbuf;
    char *outb = outbuf;
    size_t inroom, outroom, r;

    if (ch == SCM_CHAR_INVALID) return -1;
    if (ucsconv.char2ucs == NULL) return -1;
    inroom = SCM_CHAR_NBYTES(ch);
    outroom = 6;
    SCM_CHAR_PUT(inbuf, ch);
    (void)SCM_INTERNAL_MUTEX_LOCK(ucsconv.mutex);
    r = jconv(ucsconv.char2ucs, &inb, &inroom, &outb, &outroom);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ucsconv.mutex);
    if (r == INPUT_NOT_ENOUGH || r == OUTPUT_NOT_ENOUGH) {
        Scm_Error("can't convert character %u to UCS4 code: implementation problem?", ch);
    } else if (r == ILLEGAL_SEQUENCE) {
        return -1;
    } else {
        unsigned char *ucp = (unsigned char*)outbuf;
        if (ucp[0] < 0x80) return (int)ucp[0];
        if (ucp[0] < 0xe0) {
            return ((ucp[0]&0x1f)<<6) + (ucp[1]&0x3f);
        }
        if (ucp[0] < 0xf0) {
            return ((ucp[0]&0x0f)<<12)
                   + ((ucp[1]&0x3f)<<6)
                   + (ucp[2]&0x3f);
        }
        if (ucp[0] < 0xf8) {
            return ((ucp[0]&0x07)<<18)
                   + ((ucp[1]&0x3f)<<12)
                   + ((ucp[2]&0x3f)<<6)
                   + (ucp[3]&0x3f);
        }
        if (ucp[0] < 0xfc) {
            return ((ucp[0]&0x03)<<24)
                   + ((ucp[1]&0x3f)<<18)
                   + ((ucp[2]&0x3f)<<12)
                   + ((ucp[3]&0x3f)<<6)
                   + (ucp[4]&0x3f);
        }
        if (ucp[0] < 0xfe) {
            return ((ucp[0]&0x01)<<30)
                   + ((ucp[1]&0x3f)<<24)
                   + ((ucp[2]&0x3f)<<18)
                   + ((ucp[3]&0x3f)<<12)
                   + ((ucp[4]&0x3f)<<6)
                   + (ucp[5]&0x3f);
        }
        return -1;
    }
#endif /*!GAUCHE_CHAR_ENCODING_UTF_8*/
}

/*====================================================================
 * Initialization
 */
extern void Scm_Init_convlib(ScmModule *module);
extern void Scm_Init_convguess(void);
SCM_EXTERN ScmChar (*Scm_UcsToCharHook)(int ucs4);
SCM_EXTERN int (*Scm_CharToUcsHook)(ScmChar ch);

void Scm_Init_libcharconv(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(charconv);
    mod = SCM_MODULE(SCM_FIND_MODULE("gauche.charconv", TRUE));
    guess.procs = NULL;
    (void)SCM_INTERNAL_MUTEX_INIT(guess.mutex);
#if   defined(GAUCHE_CHAR_ENCODING_UTF_8)
    ucsconv.ucs2char = ucsconv.char2ucs = NULL;
#elif defined(GAUCHE_CHAR_ENCODING_EUC_JP)
    ucsconv.ucs2char = jconv_open("EUCJP", "UTF-8");
    ucsconv.char2ucs = jconv_open("UTF-8", "EUCJP");
#elif defined(GAUCHE_CHAR_ENCODING_SJIS)
    ucsconv.ucs2char = jconv_open("SJIS", "UTF-8");
    ucsconv.char2ucs = jconv_open("UTF-8", "SJIS");
#else
    ucsconv.ucs2char = ucsconv.char2ucs = NULL;
#endif
    (void)SCM_INTERNAL_MUTEX_INIT(ucsconv.mutex);
    Scm_Init_convguess();
    Scm_Init_convlib(mod);
    Scm_UcsToCharHook = ucstochar;
    Scm_CharToUcsHook = chartoucs;
}
