;; :REF: 20954ebb-33e2-471d-9232-530e643ae45f
;; :ID: c55c2ac1-8036-480a-9269-0db395323d51

(define-module (website actor))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1))

(define (Actor#mk init init-tx)
  (lambda (data)
    (let ((state (init data))
          (tx init-tx))
      (lambda (msg)
        (match (tx state msg)
          (`(,reply ,next-state ,next-tx)
           (set! state next-state)
           (set! tx next-tx)
           reply)
          (result
           (raise-exception
            (format #f "Unexpected tx result. result = ~a" result))))))))

(define (Actor#send actor msg)
  (actor msg))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Actor#mk Actor#send)
