;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


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

(define (init data)
  (match data
    (`(,articles ,layout)
     (hash-for-each (lambda (_key value) (Article#check value)) articles)
     `(,articles ,(Article#index articles layout)))
    (_
     (raise-exception (format #f "Unexpected data. data = ~a" data)))))

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

(define mk (Actor#mk init tx))

(define (Db#mk articles layout _db-path)
  (mk `(,articles ,layout)))

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
