;;;
;;; Test extension srfis
;;;

(use gauche.test)

(test-start "extension srfi modules")

(include "test-srfi-13")
(include "test-srfi-19")
(include "test-srfi-43")

(test-end)
