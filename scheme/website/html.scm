;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF: ca53237e-a27a-40d0-aeae-5d4bf4fd4e2f
;; :ID: a29c5dd3-ada2-4323-a3ae-5176762c86b0

(define-module (website html))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website list)
             (website string)
             (website data))

(define (Html#mk id bvector) (Data#mk id bvector #:html))

(define (Html#? x) (and (Data#? x) (eq? (Data#type x) #:html)))

(define (Html#check x) (unless (Html#? x) (raise-exception (format #f "x is not a Html. x = ~a" x))))

(define (Html#elim func)
  (let ((action (Data#elim func)))
    (lambda (html)
      (Html#check html)
      (action html))))

(define Html#id (Html#elim (lambda (id _bvector _type) id)))

(define Html#bvector (Html#elim (lambda (_id bvector _type) bvector)))

(define Html#type (Html#elim (lambda (_id _bvector type) type)))

(define Html#repr (Html#elim (lambda (id _bvector type) (format #f "Html#mk(id=~a bvector type)" id))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Html#mk
        Html#?
        Html#check
        Html#id
        Html#bvector
        Html#repr
        Html#type)
