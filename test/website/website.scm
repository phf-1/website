;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(define-module (test website website))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules
 (srfi srfi-64)
 (ice-9 match)
 (website website)
 (website conf)
 (website reply)
 (website header)
 (rnrs bytevectors)
 (web response)
 (gcrypt hash))

(define test-name "website")
(define conf (Conf#mk #f))
(define content (Conf#content conf))
(define layout (Conf#layout conf))
(define login (Conf#login conf))
(define password (Conf#password conf))
(define js-dir (Conf#js conf))
(define css-dir (Conf#css conf))
(define expected-response (build-response #:code 200 #:headers (Header#for #:jpeg)))
(define expected-md5 #vu8(97 131 225 168 208 152 253 223 19 79 245 22 78 2 207 165))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(test-begin test-name)
(define website (Website#mk "test" content layout login password js-dir css-dir))
(test-assert "Website#hello" (Reply#= (Website#hello website) Reply#hello))
(let* ((reply (Website#resource website #f "1" "portrait.jpeg"))
       (content (Reply#content reply))
       (content-md5 (md5 content)))
  (test-assert "Website#resource"
    (and (equal? (Reply#response reply) expected-response)
         (equal? content-md5 expected-md5))))
(test-end test-name)
