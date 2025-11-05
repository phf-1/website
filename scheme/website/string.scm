;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF:
;; :ID: 75fc941d-7fbe-427a-a685-5caab2946c0b

(define-module (website string))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1))

(define (String#check s)
  (unless (string? s)
    (raise-exception (format #f "s is not a string. s = ~a" s))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export String#check)
