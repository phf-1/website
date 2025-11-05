;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF: 5a4be4d0-3e9d-4575-9632-1b790f25373f
;; :ID: 68545fb4-2bbb-4f46-b991-57c85c63f95d

(define-module (website env))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1))

(define Env#prod "prod")
(define Env#dev "dev")
(define Env#test "test")
(define Env `(,Env#prod ,Env#dev ,Env#test))
(define (Env#? x) (member x Env))
(define (Env#dev? x) (string= x Env#dev))
(define (Env#prod? x) (string= x Env#prod))
(define (Env#test? x) (string= x Env#test))
(define (Env#check x) (unless (Env#? x) (raise-exception (format #f "x is not an Env. x = ~a" x))))
(define (Env#mk x) (Env#check x) x)
(define (Env#read) (Env#mk (or (getenv "WEBSITE_ENV") Env#prod)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Env#mk
        Env#prod
        Env#dev
        Env#test
        Env#?
        Env#prod?
        Env#dev?
        Env#test?
        Env#check
        Env#read)
