;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;;;;;;;;;;;;;;;;;;;
;; Specification ;;
;;;;;;;;;;;;;;;;;;;
;;
;; x : X, just(x) : Maybe(X)
;;
;; nothing : Maybe(X)
;;
;; use : C (X → C) → Maybe(X) → C
;; use c f :≡
;;   z ↦ match z
;;     - just(x) → f x
;;     - nothing → c

;;;;;;;;;;;;;;;;;;;;
;; Implementation ;;
;;;;;;;;;;;;;;;;;;;;

(define-module (maybe)
  #:use-module (ice-9 match)
  #:export (nothing just just? nothing? maybe-use))

(define nothing #:nothing)

(define (just value)
  `(#:just ,value))

(define (nothing? x)
  (eq? x #:nothing))

(define (just? x)
  (and (pair? x) (eq? (car x) #:just)))

(define (maybe-use default fn)
  (lambda (maybe)
    (match maybe
      (#:nothing default)
      (`(#:just ,value) (fn value))
      (_ (error "Invalid maybe value: ~a" maybe)))))
