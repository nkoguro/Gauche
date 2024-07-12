;;;
;;; gauche.cgen.standalone - Create standalone binary
;;;
;;;   Copyright (c) 2014-2024  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module gauche.cgen.standalone
  (use gauche.cgen)
  (use gauche.config)
  (use gauche.process)
  (use srfi.13)
  (use srfi.42)
  (use file.util)
  (export build-standalone)
  )
(select-module gauche.cgen.standalone)

;; API
(define (build-standalone srcfile :key (outfile #f)
                                       (extra-files '())
                                       (include-dirs '())
                                       (cpp-definitions '())
                                       (header-dirs '())
                                       (library-dirs '())
                                       (feature-ids '())
                                       (keep-c-file #f)
                                       (dynamic #f))
  (receive (placeholder out.c)
      (generate-c-file srcfile dynamic (append include-dirs '("."))
                       extra-files feature-ids)
    (unwind-protect
        (compile-c-file out.c
                        (or outfile (path-sans-extension (sys-basename srcfile)))
                        dynamic
                        (map (^d #"-D\"~|d|\"") cpp-definitions)
                        (map (^d #"-I\"~|d|\"") header-dirs)
                        (map (^d #"-L\"~|d|\"") library-dirs))
      (unless keep-c-file (sys-unlink out.c))
      (sys-unlink placeholder))))

;; This creates an empty file that reserve the temporary name, and the actual
;; C file.  Returns two names.
(define (generate-c-file file dynamic incdirs extras feature-ids)
  (define outname
    (receive (oport name) (sys-mkstemp (path-sans-extension (sys-basename file)))
      (close-port oport)
      name))
  (parameterize ([cgen-current-unit
                  (make <cgen-unit>
                    :name outname
                    :preamble "/* Generated by build-standalone */"
                    :init-prologue "int main (int argc, const char *argv[]) {"
                    :init-epilogue "}")])
    (unless dynamic
      (cgen-decl "#include <gauche/static.h>"))
    (cgen-decl "#include <gauche.h>")
    (cgen-decl (format "const char *main_script = ~a;"
                       (cgen-safe-string (file->string file))))
    (if dynamic
      (cgen-init "Scm_Init(GAUCHE_SIGNATURE);")
      (cgen-init "SCM_INIT_STATIC();"))
    (unless (null? extras)
      (setup-library-table incdirs extras))
    (dolist [feat feature-ids]
      (cgen-init #"Scm_AddFeature(\"~|feat|\", NULL);"))
    (cgen-init "Scm_SimpleMain(argc, argv, main_script, 0);")
    (cgen-emit-c (cgen-current-unit)))
  (values outname (path-swap-extension outname "c")))

(define (setup-library-table incdirs extras)
  (define setup-code
    `(let1 tab (make-hash-table 'equal?)
       ,@(list-ec
          [: x extras]
          (if-let1 f (find-file-in-paths x :paths incdirs :pred file-exists?)
            `(hash-table-put! tab ,x ,(file->string f))
            (error "Can't find library file:" x)))
       (add-embedded-code-loader! tab)))
  (cgen-decl (format "const char *setup_libraries = ~a;"
                     (cgen-safe-string (write-to-string setup-code))))
  (cgen-init "Scm_EvalCString(setup_libraries, SCM_OBJ(Scm_GaucheModule()),"
             "                NULL);"))

(define (get-static-libs xdefs)
  (let* ([libs (gauche-config "--static-libs")]
         [libs (if (any #/^-D(=|\s*)\"?GAUCHE_STATIC_EXCLUDE_GDBM\"?/ xdefs)
                 (regexp-replace-all #/-lgdbm(_compat)?/ libs "")
                 libs)]
         [libs (if (any #/^-D(=|\s*)\"?GAUCHE_STATIC_EXCLUDE_MBEDTLS\"?/ xdefs)
                 (regexp-replace-all #/-lmbed\w*/ libs "")
                 libs)])
    libs))

(define (get-dynamic-libs)
  (gauche-config "-l"))

;; (gauche-config "-I") etc. returns a single string with all -I flags in it.
;; We need to split it into each flags to check the existence of the
;; directory and properly quote it.  It is a bit tricky since the path
;; may contain whitespaces.
(define (get-config-dir-flags flag)
  (string-split (gauche-config flag) #/\s(?=(\")?-[IL])/))

;; Darwin's ld doesn't like that nonexistent directory is given to
;; -L flag.  The warning message is annoying, so we filter out such flags.
;; For Windows, we have complications of two different path representations,
;; and they don't complain having nonexistent dirs, so we leave them as is
;; (but we do escape).
(define (exclude-nonexistent-dirs dir-flags)
  (define (existing-dir? flag)
    (if-let1 m (#/^-[IL]/ flag)
      (cond-expand
       [gauche.os.windows (shell-escape-string (string-trim-right flag)
                                               'windows)]
       [else (let1 path (string-trim-both (rxmatch-after m) #\")
               (and (file-exists? path)
                    (if (string-index flag #\")
                      flag
                      (shell-escape-string flag 'posix))))])
      flag))
  (string-join (filter-map existing-dir? dir-flags) " "))

(define (compile-c-file c-file outfile dynamic xdefs xincdirs xlibdirs)
  ;; TODO: We wish we could use gauche.package.compile, but currently it is
  ;; specialized to compile extension modules.  Eventually we will modify
  ;; the module so that this function can be just a one-liner
  (let ([cc (gauche-config "--cc")]
        [cflags (gauche-config "--so-cflags")]
        [defs    (string-join xdefs " ")]
        [incdirs (exclude-nonexistent-dirs
                  `(,@xincdirs ,@(get-config-dir-flags "-I")))]
        [libdirs (exclude-nonexistent-dirs
                  `(,@xlibdirs ,@(get-config-dir-flags "-L")))]
        [libs    (if dynamic
                   (get-dynamic-libs)
                   (get-static-libs xdefs))])
    (let1 cmd #"~cc ~cflags ~defs ~incdirs -o ~outfile ~c-file ~libdirs ~libs"
      (print cmd)
      (sys-system cmd))))
