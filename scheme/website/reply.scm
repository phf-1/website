;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF:
;; :ID: 58a7cc44-bcd5-4801-b8c2-db746e805c41

(define-module (website reply))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website header)
             (rnrs bytevectors)
             (web response))

(define (Reply#mk response content)
  (unless (response? response)
    (raise-exception (format #f "response is not a Response. response = ~a\n" response)))
  (unless (bytevector? content)
    (raise-exception (format #f "content is not a Bytevector. content = ~a\n" content)))
  (list #:reply response content))

(define (Reply#= r1 r2)
  (or (eq? r1 r2)
      (and (equal? (Reply#response r1) (Reply#response r2))
           (equal? (Reply#content r1) (Reply#content r2)))))

(define (Reply#elim func)
  (lambda (reply)
    (match reply
      (`(#:reply ,response ,content)
       (func response content))
      (_ (raise-exception (format #f "reply is not a Reply. reply = ~a\n" reply))))))

(define Reply#response (Reply#elim (lambda (response _content) response)))

(define Reply#content (Reply#elim (lambda (_response content) content)))

(define Reply#404
  (Reply#mk
   (build-response #:code 404 #:headers (Header#for #:text))
   (string->utf8 "No content available.")))

(define Reply#500
  (Reply#mk
   (build-response #:code 500 #:headers (Header#for #:text))
   (string->utf8 "Server error.")))

(define Reply#hello
  (Reply#mk
   (build-response #:code 200 #:headers (Header#for #:text))
   (string->utf8 "hello")))

(define Reply#todo
  (Reply#mk
   (build-response #:code 200 #:headers (Header#for #:text))
   (string->utf8 "todo")))

(define Reply#challenge
  (Reply#mk
   (build-response #:code 401 #:headers (Header#for #:auth))
   (string->utf8 "Authentication failed.")))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Reply#mk
        Reply#response
        Reply#content
        Reply#404
        Reply#hello
        Reply#challenge
        Reply#todo
        Reply#=)
