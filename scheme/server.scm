;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;;;;;;;;;;;;;;;;;;;
;; Specification ;;
;;;;;;;;;;;;;;;;;;;
;;
;; [[id:876d6839-7a43-4215-80f4-459f13c29870]]
;;
;; Given Library, parser, then Server<Library,parser> :≡ Server#mk(Library parser) :
;; Type. Given dir : LibraryDir, then server :≡
;; Server<library,parser>#mk(dir). Given port : Port, then server#start(port) starts
;; a WEB server that implements the HTTP protocol. The content is derived from a
;; Library#mk(dir).
;;
;; [[ref:4ee84d7f-12d6-4bcf-a00f-a049631063f1][LibraryDir]]
;; [[ref:f4b947d7-9ed1-4d43-9841-f041c761d58a][Library]]
;; [[ref:c4fa289d-1eed-4bd3-b61a-78159eb36aab][parser]]
;; [[ref:da7b66f8-4977-4d55-ab82-d0f2b5c072fb][HTTP protocol]]

;;;;;;;;;;;;;;;;;;;;
;; Implementation ;;
;;;;;;;;;;;;;;;;;;;;

(define-module (server)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (parser)
  #:use-module (response)
  #:use-module (library)
  #:use-module (maybe)
  #:export (Server))

(define (parts->msg library parts)
  (match parts
    ('("health")
     '(#:json ((health . "ok"))))

    ('()
     '(#:to "/00000000-0000-0000-0000-000000000000/html"))

    ('("articles")
     '(#:to "/00000000-0000-0000-0000-000000000001/html"))

    (`(,uuid-str "org")
     (let ((text (library `(#:text ,uuid-str))))
       ((maybe-use nothing (lambda (str) `(#:text ,str))) text)))

    (`(,uuid-str "html")
     (let ((html (library `(#:html ,uuid-str))))
       ((maybe-use nothing (lambda (str) `(#:html ,str))) html)))

    (`(,id-str ,data-str)
     (let ((data (library `(#:data ,id-str ,data-str))))
       ((maybe-use nothing (lambda (pair) `(#:data ,(car pair) ,(cdr pair)))) data)))

    (_ #:not-found)))

(define (Server msg)
  (match msg
    (`(#:mk ,Library ,parser)
     (letrec ((Server<Library,parser>
               (lambda (msg)
                 (match msg
                   (`(#:mk ,library #:Library)
                    (let ((handle-request
                           (lambda (request body)
                             (let ((uri (request-uri request))
                                   (method (request-method request)))
                               (match method
                                 ('GET
                                  (match (parts->msg library (parser `(#:uri ,(uri-path uri))))
                                    ((? nothing?) (Response #:NOT_FOUND))
                                    (#:not-found (Response #:NOT_FOUND))
                                    (`(#:to ,path) (Response `(#:to ,path)))
                                    (`(#:json ,obj) (Response `(#:json ,obj)))
                                    (`(#:text ,str) (Response `(#:text ,str)))
                                    (`(#:html ,str) (Response `(#:html ,str)))
                                    (`(#:data ,binary ,type) (Response `(#:data ,binary ,type)))
                                    (_ (Response #:NOT_FOUND))))
                                 (_ (Response #:BAD_REQUEST)))))))

                      ;; server : Server<Library,parser>
                      (lambda (msg)
                        (match msg
                          (`(#:start ,port)
                           (format #t "Server listening on localhost:~a~%" port)
                           (run-server handle-request 'http `(#:port ,port)))
                          (_ (error (format #f "Unexpected msg: ~a" msg)))))))

                   (`(#:mk ,dir #:Directory)
                    (let ((library (Library `(#:mk ,dir #:Directory))))
                      (Server<Library,parser> `(#:mk ,library #:Library))))

                   (_ (error (format #f "Unexpected msg: ~a" msg)))))))
       Server<Library,parser>))
    (_ (error (format #f "Unexpected msg: ~a" msg)))))
