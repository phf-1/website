;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF: ac0f68a7-24d6-458e-92b4-68f700a1cdfb
;; :ID: acc4f5dd-af32-46f7-a036-021faef3dd6d

(define-module (website verb))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1))

(define Verb#get 'GET)
(define Verb#post 'POST)
(define (Verb#= v1 v2) (eq? v1 v2))
(define Verb `(,Verb#get))
(define (Verb#? x) (member x Verb))
(define (Verb#get? x) (string= x Verb#get))
(define (Verb#check x) (unless (Verb#? x) (raise-exception (format #f "x is not an Verb. x = ~a" x))))
(define (Verb#mk x) (Verb#check x) x)

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Verb#mk
        Verb#get
        Verb#post
        Verb#?
        Verb#=
        Verb#get?
        Verb#check)
