;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF: 3fafced7-5be5-4297-b74a-f669326821c1
;; :ID: 791d5f9d-ada3-4349-aadc-ba8e4e81411a

(define-module (website text))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website list)
             (website string)
             (website data))

(define (Text#mk id bvector) (Data#mk id bvector #:text))

(define (Text#? x) (and (Data#? x) (eq? (Data#type x) #:text)))

(define (Text#check x) (unless (Text#? x) (raise-exception (format #f "x is not a Text. x = ~a" x))))

(define (Text#elim func)
  (let ((action (Data#elim func)))
    (lambda (text)
      (Text#check text)
      (action text))))

(define Text#id (Text#elim (lambda (id _bvector _type) id)))

(define Text#bvector (Text#elim (lambda (_id bvector _type) bvector)))

(define Text#type (Text#elim (lambda (_id _bvector type) type)))

(define Text#repr (Text#elim (lambda (id _bvector type) (format #f "Text#mk(id=~a bvector type)" id))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Text#mk
        Text#?
        Text#check
        Text#id
        Text#bvector
        Text#repr
        Text#type)
