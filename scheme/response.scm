;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;;;;;;;;;;;;;;;;;;;
;; Specification ;;
;;;;;;;;;;;;;;;;;;;
;;
;; [[id:bb17a03b-7429-477b-b304-e509cbb44485]]
;;
;; Response is a procedure that constructs HTTP responses for the web server.

;;;;;;;;;;;;;;;;;;;;
;; Implementation ;;
;;;;;;;;;;;;;;;;;;;;

(define-module (response)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (rnrs bytevectors)
  #:export (Response))

(define (Response msg)
  (match msg
    (`(#:json ,alist)
     (let ((body (scm->json-string alist)))
       (values
        (build-response
         #:code 200
         #:headers `((content-type . (application/json))))
        body)))

    (`(#:html ,content)
     (values
      (build-response
       #:code 200
       #:headers `((content-type . (text/html))))
      content))

    (`(#:text ,content)
     (values
      (build-response
       #:code 200
       #:headers `((content-type . (text/plain))))
      content))

    (`(#:data ,binary ,type)
     (let ((mime
            (match type
              (#:pdf '(application/pdf))
              (#:png '(image/png))
              (#:jpg '(image/jpeg))
              (#:woff2 '(font/woff2))
              (_ '(application/octet-stream)))))
       (values
        (build-response
         #:code 200
         #:headers `((content-type . ,mime)))
        binary)))

    (`(#:to ,path)
     (values (build-response #:code 301
                             #:headers `((location . ,(build-relative-ref #:path path))))
             ""))

    (#:NOT_FOUND
     (let ((body "<!DOCTYPE html>\n<html><body><h1>404 Not Found</h1></body></html>"))
       (values
        (build-response
         #:code 404
         #:headers `((content-type . (text/html))))
        body)))

    (#:BAD_REQUEST
     (let ((body "<!DOCTYPE html>\n<html><body><h1>400 Bad Request</h1></body></html>"))
       (values
        (build-response
         #:code 400
         #:headers `((content-type . (text/html))))
        body)))

    (_ (error (format #f "Unexpected msg: ~a" msg)))))
