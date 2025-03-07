;;;
;;; windows.stub - windows api bridge
;;;
;;;   Copyright (c) 2010-2025  Shiro Kawai  <shiro@acm.org>
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

(in-module os.windows)

(inline-stub
(.include "gauche/extend.h")
(.when (defined GAUCHE_WINDOWS)

;; The following bindings are provided in the core:
;;  <win:handle>
;;  sys-win-process?
;;  sys-win-process-pid
;;  sys-get-osfhandle

;;====================================================================
;; Fileapi
;;

(define-cproc sys-get-disk-free-space-ex (path::<const-cstring>)
  (let* ([wpath::LPTSTR (SCM_MBS2WCS path)]
         [avail::ULARGE_INTEGER]
         [total::ULARGE_INTEGER]
         [tfree::ULARGE_INTEGER]
         [r::BOOL (GetDiskFreeSpaceEx wpath (& avail) (& total) (& tfree))])
    (when (== r 0) (Scm_SysError "GetDiskFreeSpaceEx failed on path %s" path))
    (return (SCM_LIST3 (Scm_OffsetToInteger (ref avail QuadPart))
                       (Scm_OffsetToInteger (ref total QuadPart))
                       (Scm_OffsetToInteger (ref tfree QuadPart))))))

;;====================================================================
;; MessageBox
;;

;; flags for buttons
(define-enum MB_ABORTRETRYIGNORE)
(define-enum MB_CANCELTRYCONTINUE)
(define-enum MB_HELP)
(define-enum MB_OK)
(define-enum MB_OKCANCEL)
(define-enum MB_RETRYCANCEL)
(define-enum MB_YESNO)
(define-enum MB_YESNOCANCEL)

;; flags for icons
(define-enum MB_ICONEXCLAMATION)
(define-enum MB_ICONWARNING)
(define-enum MB_ICONINFORMATION)
(define-enum MB_ICONASTERISK)
(define-enum MB_ICONQUESTION)
(define-enum MB_ICONSTOP)
(define-enum MB_ICONERROR)
(define-enum MB_ICONHAND)

;; flags for default button
(define-enum MB_DEFBUTTON1)
(define-enum MB_DEFBUTTON2)
(define-enum MB_DEFBUTTON3)
(define-enum MB_DEFBUTTON4)

;; flags for modality
(define-enum MB_APPLMODAL)
(define-enum MB_SYSTEMMODAL)
(define-enum MB_TASKMODAL)

;; flags for other options
(define-enum MB_DEFAULT_DESKTOP_ONLY)
(define-enum MB_RIGHT)
(define-enum MB_RTLREADING)
(define-enum MB_SETFOREGROUND)
(define-enum MB_TOPMOST)
(define-enum MB_SERVICE_NOTIFICATION)

;; return values
(define-enum IDABORT)
(define-enum IDCANCEL)
(define-enum IDCONTINUE)
(define-enum IDIGNORE)
(define-enum IDNO)
(define-enum IDOK)
(define-enum IDRETRY)
(define-enum IDTRYAGAIN)
(define-enum IDYES)

(define-cproc sys-message-box (window
                               text::<const-cstring>?
                               :optional (caption::<const-cstring>? #f)
                                         (type::<uint> 0))
  ::<int>
  (let* ([h::HANDLE NULL]
         [wtext::(const TCHAR*) NULL]
         [wcaption::(const TCHAR*) NULL]
         [r::int 0])
    (cond [(SCM_FALSEP window) (= h NULL)]
          [(Scm_WinHandleP window '#f) (= h (Scm_WinHandle window '#f))]
          [else (SCM_TYPE_ERROR window "<win:handle> or #f")])
    (when text    (= wtext (SCM_MBS2WCS text)))
    (when caption (= wcaption (SCM_MBS2WCS caption)))
    (= r (MessageBox h wtext wcaption type))
    (when (== r 0) (Scm_SysError "MessageBox failed"))
    (result r)))


); defined(GAUCHE_WINDOWS)

); inline-stub

;; Local variables:
;; mode: scheme
;; end:
