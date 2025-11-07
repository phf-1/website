;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF: 65e02573-e1ea-4463-95bc-46ce866426af
;; :ID: 7558b5be-0b94-41fe-9e6a-e1230919f764

(define-module (website server))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (web request)
             (web uri)
             (rnrs bytevectors)
             (gcrypt base64)
             (website identity)
             (web response)
             (website website)
             (website verb)
             (website utils)
             (website db)
             (website string)
             (website reply)
             (web server))

(define (request->identity request)
  (define headers (request-headers request))
  (define auth-header (assq-ref headers 'authorization))
  (if (not auth-header)
      #f
      (match auth-header
        (`(basic . ,base64)
         (catch #t
           (lambda ()
             (define decoded (base64-decode base64))
             (define creds-str (utf8->string decoded))
             (define creds (string-split creds-str #\:))
             (match creds
               (`(,login ,pwd)
                (Identity#mk login pwd))
               (else #f)))
           (lambda (key . args)
             #f)))
        (else #f))))

(define body-max-length 6000) ;; : Number of bytes.

(define (split-first str ch)
  (let ((pos (string-index str ch)))
    (if pos
        (cons (substring str 0 pos)
              (substring str (+ pos 1)))
        (cons str ""))))

(define (parse-urlencoded str)
  (if (or (not str) (string-null? str))
      '()
      (let ((pairs (string-split str #\&)))
        (filter pair?
                (map (lambda (p)
                       (and (not (string-null? p))
                            (let* ((kv (split-first p #\=))
                                   (k (uri-decode (car kv)))
                                   (v (uri-decode (cdr kv))))
                              (cons k v))))
                     pairs)))))

(define (row-mk epoch email path content)
  (Nat#check epoch)
  (String#check email)
  (unless (< (string-length email) 100)
    (raise-exception "email is too long"))
  (String#check path)
  (unless (< (string-length path) 100)
    (raise-exception "path is too long"))
  (String#check content)
  (unless (< (string-length content) 5000)
    (raise-exception "content is too long"))
  `(#:row ,epoch ,email ,path ,content))


(define (params->handler env content layout login password js-dir css-dir db-path)
  (define website (Website#mk env content layout login password js-dir css-dir db-path))
  ;; TODO: (define db (Db#mk db-path))
  (lambda (request request-body)
    (let ((verb (request-method request))
          (path (split-and-decode-uri-path (uri-path (request-uri request))))
          (identity (request->identity request))
          (reply #f))
      (set! reply
            (cond
             ((Verb#= verb Verb#get)
              (match path
                ('("hello")
                 (Website#hello website))

                (`("static" ,name)
                 (Website#static website name))

                (`("article" ,id "html")
                 (Website#article website identity id #:html))

                (`("article" ,id "org")
                 (Website#article website identity id #:org))

                (`("article" ,id ,res-id)
                 (Website#resource website identity id res-id))

                ('("articles")
                 (Website#index website))

                ('()
                 (Website#article website identity "1" #:html))

                (`(,res-id)
                 (Website#resource website identity "1" res-id))

                (_
                 (Website#404 website))))
             ;; ((Verb#= verb Verb#post)
             ;;  (match path
             ;;    ((? (lambda (_x) (< (bytevector-length request-body) body-max-length)) `("article" ,id "html"))
             ;;     (let ((email #f) (content #f) (epoch (current-time)) (data #f) (row #f))
             ;;       (set! data (parse-urlencoded (utf8->string request-body)))
             ;;       (set! email (assoc-ref data "email"))
             ;;       (set! content (assoc-ref data "content"))
             ;;       (set! path (string-join path "/"))
             ;;       (with-exception-handler
             ;;           (lambda (exn)
             ;;             (Website#500 website))
             ;;         (lambda ()
             ;;           (set! row (row-mk epoch email path content))
             ;;           (Db#save db row)
             ;;           (Reply#mk
             ;;            (build-response
             ;;             #:code 200
             ;;             #:headers '((content-type . (text/plain (charset . "utf-8")))))
             ;;            (string->utf8 ""))))))

             ;;    (_
             ;;     (Website#404 website))))
             (#t
              (Website#404 website))))
      (values (Reply#response reply) (Reply#content reply)))))

(define (Server#mk env ip port content layout login password js-dir css-dir db-path)
  (let ((handler (params->handler env content layout login password js-dir css-dir db-path)))
    (run-server handler
                'http
                `(#:addr ,(inet-pton AF_INET ip)
                  #:port ,(string->number port)))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Server#mk)
