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
             (website website)
             (website verb)
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

(define (params->handler env content layout login password)
  (define website (Website#mk env content layout login password))
  (lambda (request _request-body)
    (let ((verb (request-method request))
          (path (split-and-decode-uri-path (uri-path (request-uri request))))
          (identity (request->identity request))
          (reply #f))
      (set! reply
            (if (Verb#= verb Verb#get)
                (match path
                  ('("hello")
                   (Website#hello website))

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
                   (Website#404 website)))
                (Website#404 website)))
      (values (Reply#response reply) (Reply#content reply)))))

(define (Server#mk env ip port content layout login password)
  (let ((handler (params->handler env content layout login password)))
    (run-server handler
                'http
                `(#:addr ,(inet-pton AF_INET ip)
                  #:port ,(string->number port)))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Server#mk)
