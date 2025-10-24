(define-module (test website actor))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules
 (srfi srfi-64)
 (ice-9 match)
 (website actor))

(define (nat? x)
  (and (integer? x) (<= 0 x)))

(define (nat-check x)
  (unless (nat? x)
    (raise-exception
     (format #f "time is not a Nat. time = ~a" time))))

(define test-name "actor")

(define (init time)
  (nat-check time)
  time)

(define (tx state message)
  (let ((time state))
    (match message
      (#:incr `(#t ,(+ time 1) ,tx))
      (#:read `(,time ,time ,tx)))))

(define Clock#mk (Actor#mk init tx))

(define (Clock#incr clock)
  (Actor#send clock #:incr))

(define (Clock#read clock)
  (Actor#send clock #:read))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(test-begin test-name)
(define clock (Clock#mk 0))
(test-assert "Clock#read" (eqv? (Clock#read clock) 0))
(test-assert "Clock#incr" (Clock#incr clock))
(test-assert "Clock#read" (eqv? (Clock#read clock) 1))
(test-end test-name)
