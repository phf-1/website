;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF:
;; :ID: 07e20008-0dfe-49ed-8d92-7e2eb2585e16

(define-module (website utils))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 format)
             (ice-9 pretty-print)
             (website string))

(define-macro (pvar var)
  (let* ((name (symbol->string var))
         (format-str (string-append name " = ~a\n")))
    `(begin (display (format #f ,format-str (object->string ,var pretty-print))) ,var)))

(define (Nat#? x)
  (and (integer? x) (<= 0 x)))

(define (Nat#check x)
  (unless (Nat#? x)
    (raise-exception (format #f "x is not a Nat. x = ~a" x))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export pvar
        Nat#?
        Nat#check)
