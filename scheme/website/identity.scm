;; :REF:
;; :ID: 21cfd276-8516-46e4-8c00-94d2c8cbc1bd

(define-module (website identity))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website string))

(define (Identity#mk login pwd)
  (String#check login)
  (String#check pwd)
  `(#:identity ,login ,pwd))

(define (Identity#? x)
  (and (list? x) (eq? (car x) #:identity)))

(define (Identity#elim func)
  (lambda (data)
    (match data
      (`(#:identity ,login ,pwd)
       (func login pwd))
      (_
       (raise-exception (format #f "Unexpected data. data = ~a" data))))))

(define Identity#login
  (Identity#elim (lambda (login _pwd) login)))

(define Identity#pwd
  (Identity#elim (lambda (_login pwd) pwd)))

(define (Identity#= i1 i2)
  (and (string= (Identity#login i1) (Identity#login i2))
       (string= (Identity#pwd i1) (Identity#pwd i2))))

(define (Identity#check x)
  (unless (Identity#? x)
    (raise-exception (format #f "x is not an Identity. x = ~a" x))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Identity#mk
        Identity#login
        Identity#pwd
        Identity#?
        Identity#=
        Identity#check)
