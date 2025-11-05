;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF: 4eb783af-fe6e-4a18-85d8-5783ea1fd1c2
;; :ID: 805a6b11-9dfc-413a-ae86-f6aedb371384

(define-module (website data))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website list)
             (website string)
             (rnrs bytevectors))

(define (Data#mk id bvector type)
  (String#check id)
  (unless (bytevector? bvector)
    (raise-exception (format #f "bvector is not a Bytevector. bvector = ~a" bvector)))
  (unless (keyword? type)
    (raise-exception (format #f "type is not a Keyword. type = ~a" type)))
  `(#:data ,id ,bvector ,type))

(define (Data#? x) (and (list? x) (eq? (car x) #:data)))

(define (Data#check x) (unless (Data#? x) (raise-exception (format #f "x is not a Data. x = ~a" x))))

(define (Data#elim func)
  (lambda (data)
    (Data#check data)
    (match data
      (`(#:data ,id ,bvector ,type)
       (func id bvector type)))))

(define Data#id (Data#elim (lambda (id _bvector _type) id)))

(define Data#bvector (Data#elim (lambda (_id bvector _type) bvector)))

(define Data#type (Data#elim (lambda (_id _bvector type) type)))

(define Data#repr (Data#elim (lambda (id _bvector type) (format #f "Data#mk(id=~a bvector type=~a)" id type))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Data#mk
        Data#?
        Data#check
        Data#id
        Data#elim
        Data#bvector
        Data#repr
        Data#type)
