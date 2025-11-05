;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF:
;; :ID: 805a6b11-9dfc-413a-ae86-f6aedb371384

(define-module (website jpeg))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website list)
             (website string)
             (website data))

(define (Jpeg#mk id bvector) (Data#mk id bvector #:jpeg))

(define (Jpeg#? x) (and (Data#? x) (eq? (Data#type x) #:jpeg)))

(define (Jpeg#check x) (unless (Jpeg#? x) (raise-exception (format #f "x is not a Jpeg. x = ~a" x))))

(define (Jpeg#elim func)
  (let ((action (Data#elim func)))
    (lambda (jpeg)
      (Jpeg#check jpeg)
      (action jpeg))))

(define Jpeg#id (Jpeg#elim (lambda (id _bvector _type) id)))

(define Jpeg#bvector (Jpeg#elim (lambda (_id bvector _type) bvector)))

(define Jpeg#type (Jpeg#elim (lambda (_id _bvector type) type)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Jpeg#mk
        Jpeg#?
        Jpeg#check
        Jpeg#id
        Jpeg#bvector
        Jpeg#type)
