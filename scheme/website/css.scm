;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF:
;; :ID: 3c486a14-b906-4208-84a6-daada733f683

(define-module (website css))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website list)
             (website string)
             (website data))

(define (Css#mk id bvector) (Data#mk id bvector #:css))

(define (Css#? x) (and (Data#? x) (eq? (Data#type x) #:css)))

(define (Css#check x) (unless (Css#? x) (raise-exception (format #f "x is not a Css. x = ~a" x))))

(define (Css#elim func)
  (let ((action (Data#elim func)))
    (lambda (css)
      (Css#check css)
      (action css))))

(define Css#id (Css#elim (lambda (id _bvector _type) id)))

(define Css#bvector (Css#elim (lambda (_id bvector _type) bvector)))

(define Css#type (Css#elim (lambda (_id _bvector type) type)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Css#mk
        Css#?
        Css#check
        Css#id
        Css#bvector
        Css#type)
