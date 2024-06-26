;;;
;;; Generate genconfig.in
;;;

(use srfi.13)
(use util.match)

;;
;; gen-genconfig.scm
;;         + <---- genconfig.in.in : template
;;         |
;;         |     release tarball creation
;;         v
;;    genconfig.in    doc/gauche-config.1.in  doc/man-gauche-config.texi
;;         |                 |
;;         |     configure   |
;;         v                 v
;;     genconfig       doc/gauche/config.1
;;         |
;;         |     make
;;         v
;;   gauche-config.c
;;    paths_arch.c
;;  lib/gauche/config.scm
;;

;; This script should depend on _released_ Gauche.  Do not use pre-release
;; features.

;; To add a new option to gauche-config, edit *config-parameters*.
;;
;;  <config-parameter> : <section> | <entry>
;;  <section> : <string>     ; used in help messaeg
;;  <entry> : (((<option> <source-string>) ...) <help-message>)
(define *config-parameters*
  '("General parameter"
    ((("-V" "$gauche_version"))
     "The current Gauche version.")

    "Parameters to compile applications using Gauche"
    ((("-I" "-I$gauche_incdir"))
     "Include path options required to compile programs using Gauche\n\
      (Note: This doesn't work if Gauche installation directory\n\
      path contains whitespaces.  See --incdirs below.)")
    ((("-L" "-L$gauche_archdir $local_lib"))
     "Library path options required to link programs using Gauche\n\
      (Note: This doesn't work if Gauche installation directory\n\
      path contains whitespaces.  See --archdirs below.)")
    ((("-l" "-l$garchabi $gauche_libs"))
     "Link library options required to link programs using Gauche.")
    ((("--cc" "$cc"))
     "The name of the compiler used to compile this Gacuhe.")
    ((("--cpp" "$cpp"))
     "The command to run the C preprocessor.")
    ((("--ac" "$gauche_aclocaldir"))
     "The directory that contains Gauche-specific autoconf macros.")
    ((("--reconfigure" "./configure $gauche_configure_args"))
     "The command line used to configure the current installation.")
    ((("--arch" "$arch"))
     "The autoconf-style architecture signature (cpu-vendor-kernel-os).")
    ((("--incdirs" "$incdirs")
      ("--archdirs" "$archdirs"))
     "The list of directory names to be looked for include files and\n\
      libraries, respectively.  Each directory name may be quoted if\n\
      it contains whitespaces, and separated by ':' on Unix platforms, or\n\
      by ';' on Windows platforms.")
    ((("--local-incdir" "$local_inc")
      ("--local-libdir" "$local_lib"))
     "These are '-I' and '-L' flags for additional local headers/libraries\n\
      to search, given by '--with-local' configure flags.  Note that those\n\
      are also included in '-I', '-L', '--incdirs', and '--archdirs'.")

    "Parameters to install files"
    ((("--prefix" "$prefix"))
     "The directory prefix set by configure.")
    ((("--sysincdir" "$gauche_incdir")
      ("--siteincdir" "$gauche_siteincdir")
      ("--pkgincdir" "$gauche_pkgincdir"))
     "Directories where system|site|package header files of extensions go.")
    ((("--syslibdir" "$gauche_libdir")
      ("--sitelibdir" "$gauche_sitelibdir")
      ("--pkglibdir" "$gauche_pkglibdir"))
     "Directories where system|site|package scheme files go.")
    ((("--sysarchdir" "$gauche_archdir")
      ("--sitearchdir" "$gauche_sitearchdir")
      ("--pkgarchdir" "$gauche_pkgarchdir"))
     "Directories where system|site|package DSO files go.")
    ((("--mandir" "$mandir")
      ("--infodir" "$infodir"))
     "Directories where gauche manpage and info docs are installed.")

    "Parameters to help building extensions"
    ((("--object-suffix" "$object_suffix"))
     "The extension of the compiled objects (e.g. 'o' or 'obj).")
    ((("--executable-suffix" "$executable_suffix"))
     "The extension of the executable including a period (empty on Unix systems,\n\
      '.exe' on Windows.")
    ((("--so-suffix" "$shlib_so_suffix"))
     "The extension for dynamically loadable (dlopen-able) modules (e.g. 'so').")
    ((("--so-cflags" "$shlib_so_cflags"))
     "Additional CFLAGS to create dynamically loadable modules.")
    ((("--so-ldflags" "$shlib_so_ldflags"))
     "Additional LDFLAGS to create dynamically loadable modules.")
    ((("--so-libs" "$shlib_so_libs"))
     "Additional libraries required to create dynamically loadable modules.")
    ((("--dylib-suffix" "$shlib_dylib_suffix"))
     "The extension for dynamically linked libraries (as opposed to dlopen()ed)\n\
      Usually the same as --so-suffix, but OSX wants 'dylib'.")
    ((("--dylib-ldflags" "$shlib_dylib_ldflags"))
     "LDFLAGS to create dynamically linked libraries.")
    ((("--rpath-flag" "$rpath_flag"))
     "Compiler flag(s) to embed RPATH")
    ((("--default-cflags" "$default_cflags"))
     "This is the CFLAGS used to compile Gauche, and to be used to compile\n\
      extensions as well.")
    ((("--cppflags" "$cppflags"))
     "Compiler flag(s) passed to C preprocessor")
    ((("--static-libs" "$static_libs"))
     "List of library link flags ('-llib') required to link Gauche statically.\n\
      Similar to '-l', but this includes the libraries that are used\n\
      for extension modules, and also the static library itself,\n\
      that is -lgauche-static-X.X.")
    ((("--libgauche-so" "$libgauche_so"))
     "The base name of dynamically linked libgauche.")

    )) ; end of *config-parameters*

;;
;; Generating genconfig.in from genconfig.in.in
;;

(define (generate genconfig.in.in)
  (define (process-line line)
    (cond
     [(equal? line "@@@USAGE@@@") (gen-usage)]
     [(equal? line "@@@COMMAND_TABLE@@@") (gen-command-table)]
     [(equal? line "@@@COMMAND_LIST@@@") (gen-command-list)]
     [(string-prefix? "@@@" line)]
     [else (print line)]))

  (call-with-input-file genconfig.in.in
    (^p (for-each process-line (port->string-list p)))))

(define (gen-usage)
  (dolist [e *config-parameters*]
    (match e
      [(? string?) (format #t "    \"\\n\"\n    \"~a\\n\"\n" e)]
      [(((opt src) ...) help)
       ;; TRANSIENT: After 1.0 release, we can switch to use text.fill
       ;; (let1 s `(,(map (cut format "  ~a\n" <>) (drop-right opt 1))
       ;;           ,($ tree->string $ text->filled-stree help
       ;;               :indent 8
       ;;               :lead-in (format "  ~a" (last opt))
       ;;               :width 78))
       ;;   (dolist [h (string-split s #\newline)]
       ;;     (format #t "    \"~a\\n\"\n" h)))
       (dolist [o opt]
         (format #t "    \"  ~a\\n\"\n" o))
       (dolist [h (string-split help #\newline)]
         (format #t "    \"        ~a\\n\"\n" h))])))

(define (gen-command-table)
  (dolist [e *config-parameters*]
    (match e
      [(? string?) #f]
      [(((opt src) ...) help)
       (for-each (^[o s] (format #t "    {~s, ~s},\n" o s)) opt src)])))

(define (gen-command-list)
  (dolist [e *config-parameters*]
    (match e
      [(? string?) #f]
      [(((opt src) ...) help)
       (for-each (^[o s] (format #t "    (~s ~s)\n" o s)) opt src)])))

;; Generate info fragment to be included from doc/program.texi
(define (gen-info)
  (print "@c To be included from program.texi")
  (print "@c Generated by gen-genconfig.scm.  DO NOT EDIT.")
  (fold (^[e state]
          (match e
            [(? string?)
             (unless (eq? state 'beginning)
               (print "@end table") (print))
             (format #t "~a:\n\n@table @option\n" e)
             'cont]
            [(((opt src) ...) help)
             (format #t "@item ~a\n" (car opt))
             (dolist [o (cdr opt)]
               (format #t "@itemx ~a\n" o))
             (print (regexp-replace #/@/ help "@@"))
             'cont]))
        'beginning *config-parameters*)
  (print "@end table"))

;; Generate manpage
(define (gen-man)
  (print ".\\\" -*-nroff-*-")
  (print ".TH GAUCHE\\-CONFIG \"1\" \"\" \"Gauche @GAUCHE_VERSION@\" \"Gauche Commands\"")
  (print ".SH NAME")
  (print "gauche-config \\- retrieve configuration parameters of Gauche")
  (print ".SH SYNOPSIS")
  (print ".B gauche-config")
  (print ".I option")
  (print ".br")
  (print ".sp 0.3")
  (print ".SH DESCRIPTION")
  (print ".I Gauche-config")
  (print "displays various parameters specified at the configuration time")
  (print "of the Gauche Scheme implementation.  It can be used in Makefile")
  (print "and other configuration scripts that uses Gauche.")
  (print)
  (print ".SH OPTIONS")

  (dolist [e *config-parameters*]
    (match e
      [(? string?)
       (print)
       (print ".SS " e)]
      [(((opt src) ...) help)
       (print ".TP")
       (print ".B " (string-join opt ", "))
       (print help)]))

  (print)
  (print ".SH AUTHORS")
  (print "Shiro Kawai (shiro @ acm . org)")
  (print)
  (print ".SH SEE ALSO")
  (print "gosh(1), gauche-package(1)")
  (print ".PP")
  (print "Gauche Scheme script engine:")
  (print ".br")
  (print "https://practical-scheme.net/gauche/"))


(define (main args)
  (match (cdr args)
    [(file)
     (with-output-to-file "genconfig.in"
       (cut generate file))
     (with-output-to-file "../doc/man-gauche-config.texi"
       (cut gen-info))
     (with-output-to-file "../doc/gauche-config.1.in"
       (cut gen-man))
     0]
    [_ (exit 1 "Usage: gosh gen-genconfig.scm <genconfig-template>")]))
