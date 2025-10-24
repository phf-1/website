;; :REF: 6ea23d0d-78c4-468c-9998-eaa555ecdb12
;; :ID: 0b9c4f86-bcd8-498f-979e-c84b9196ab2d

(define-module (website conf))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website actor))

(define (init _data)
  #f)

(define (tx state msg)
  (match msg
    (#:ip
     `(,(or (getenv "WEBSITE_IP") "127.0.0.1") ,state ,tx))

    (#:port
     `(,(or (getenv "WEBSITE_PORT") "3000") ,state ,tx))

    (#:content
     `(,(getenv "WEBSITE_CONTENT") ,state ,tx))      

    (#:layout
     `(,(getenv "WEBSITE_LAYOUT") ,state ,tx))      

    (#:env
     `(,(getenv "WEBSITE_ENV") ,state ,tx))

    (#:login
     `(,(getenv "WEBSITE_LOGIN") ,state ,tx))

    (#:password
     `(,(getenv "WEBSITE_PASSWORD") ,state ,tx))      

    (_
     (raise-exception (format #f "Unexpected message. msg = ~a" msg)))))

(define Conf#mk (Actor#mk init tx))

(define (Conf#ip conf)
  (Actor#send conf #:ip))

(define (Conf#port conf)
  (Actor#send conf #:port))

(define (Conf#content conf)
  (Actor#send conf #:content))

(define (Conf#layout conf)
  (Actor#send conf #:layout))

(define (Conf#env conf)
  (Actor#send conf #:env))

(define (Conf#login conf)
  (Actor#send conf #:login))

(define (Conf#password conf)
  (Actor#send conf #:password))

(define (Conf#repr conf)
  (Actor#send conf #:repr))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Conf#mk
        Conf#ip
        Conf#port
        Conf#content
        Conf#layout        
        Conf#env
        Conf#login
        Conf#password)
