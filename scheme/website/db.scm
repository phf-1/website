;; :REF: ff90b350-b368-4cb3-b29e-d34e8fd341a3
;; :ID: 0b9c4f86-bcd8-498f-979e-c84b9196ab2d

(define-module (website db))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website actor)
             (website article))

(define (init articles)
  (hash-for-each (lambda (_key value) (Article#check value)) articles)
  `(,articles ,(Article#index articles)))

(define (tx state msg)
  (match-let ((`(,articles ,index) state))
    (match msg
      (`(#:article "index")
       `(,index ,state ,tx))

      ((and `(#:article ,id) (string? id))
       `(,(hash-ref articles id) ,state ,tx))

      (#:repr
       `(,(format #f "Db#mk(~a articles)" (length articles)) ,state ,tx))

      (_
       (raise-exception (format #f "Unexpected message. msg = ~a" msg))))))

(define Db#mk (Actor#mk init tx))

(define (Db#article db id)
  (Actor#send db `(#:article ,id)))

(define (Db#repr db)
  (Actor#send db #:repr))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Db#mk
        Db#article
        Db#repr)
