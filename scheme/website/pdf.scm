;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF:
;; :ID: 8e55d5f6-0128-49d8-b0fe-b9f740862ac0

(define-module (website pdf))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website list)
             (website string)
             (website data))

(define (Pdf#mk id bvector) (Data#mk id bvector #:pdf))

(define (Pdf#? x) (and (Data#? x) (eq? (Data#type x) #:pdf)))

(define (Pdf#check x) (unless (Pdf#? x) (raise-exception (format #f "x is not a Pdf. x = ~a" x))))

(define (Pdf#elim func)
  (let ((action (Data#elim func)))
    (lambda (pdf)
      (Pdf#check pdf)
      (action pdf))))

(define Pdf#id (Pdf#elim (lambda (id _bvector _type) id)))

(define Pdf#bvector (Pdf#elim (lambda (_id bvector _type) bvector)))

(define Pdf#type (Pdf#elim (lambda (_id _bvector type) type)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Pdf#mk
        Pdf#?
        Pdf#check
        Pdf#id
        Pdf#bvector
        Pdf#type)
