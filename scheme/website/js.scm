;; :REF:
;; :ID: ec280346-90e9-486e-8985-8badf50744e5

(define-module (website js))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website list)
             (website string)
             (website data))

(define (Js#mk id bvector) (Data#mk id bvector #:js))

(define (Js#? x) (and (Data#? x) (eq? (Data#type x) #:js)))

(define (Js#check x) (unless (Js#? x) (raise-exception (format #f "x is not a Js. x = ~a" x))))

(define (Js#elim func)
  (let ((action (Data#elim func)))
    (lambda (js)
      (Js#check js)
      (action js))))

(define Js#id (Js#elim (lambda (id _bvector _type) id)))

(define Js#bvector (Js#elim (lambda (_id bvector _type) bvector)))

(define Js#type (Js#elim (lambda (_id _bvector type) type)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Js#mk
        Js#?
        Js#check
        Js#id
        Js#bvector
        Js#type)
