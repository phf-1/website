;; :REF:
;; :ID:

(define-module (website website))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (gcrypt base64)
             (ice-9 ftw)
             (ice-9 match)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-26)
             (web request)
             (web response)
             (website actor)
             (website article)
             (website data)
             (website db)
             (website directory-and-article)
             (website header)
             (website html)
             (website identity)
             (website jpeg)
             (website list)
             (website path)
             (website pdf)
             (website reply)
             (website string)
             (website text)
             (website verb))


(define (data->headers data)
  (Data#check data)
  (cond
   ((Text#? data) (Header#for #:text))
   ((Jpeg#? data) (Header#for #:jpeg))
   ((Pdf#? data) (Header#for #:pdf))
   ((Html#? data) (Header#for #:html))
   (#t (raise-exception (format #f "Unexpected data SubType. data = ~a" data)))))

(define (article->reply article res-id)
  (match res-id
    ("article.html"
     (cons
      (build-response #:code 200 #:headers (Header#for #:html))
      (Html#bvector (Article#html article))))
    ("article.org"
     (cons
      (build-response #:code 200 #:headers (Header#for #:text))
      (Text#bvector (Article#org article))))
    (_
     (match (Article#data article res-id)
       (#f Reply#404)
       (data
        (cons
         (build-response #:code 200 #:headers (data->headers data))
         (Data#bvector data)))))))

(define (send-challenge? article identity expected-identity)
  (and (Article#private? article)
       (or (not (Identity#? identity))
           (not (Identity#= identity expected-identity)))))

(define (identity-check identity)
  (unless (or (eq? identity #f) (Identity#? identity))
    (raise-exception (format #f "identity is not #f nor an Identity. identity = ~a" identity))))

(define (type-check type)
  (define types '(#:html #:org))
  (unless (memq type types)
    (raise-exception (format #f "type is not a type. types = ~a. type = ~a" types type))))

(define (init data)
  (match data
    (`(,env ,content ,layout ,login ,password)
     (display "TODO: check env\n")
     (Path#directory-check content)
     (Path#directory-check layout)
     (String#check login)
     (String#check password)
     (let ((db (Db#mk (contentdirectory->articles content)))
           (identity (Identity#mk login password)))
       `(,db ,identity)))
    (_
     (raise-exception (format #f "Unexpected data. data = ~a" data)))))

(define (tx state message)
  (match-let ((`(,db ,expected-identity) state))
    (match message
      (#:hello
       `(,Reply#hello ,state ,tx))

      (#:x404
       `(,Reply#404 ,state ,tx))

      (#:index
       (tx state '(#:article #f "index" #:html)))

      (`(#:article ,identity ,id ,type)
       (identity-check identity)
       (String#check id)
       (match (Db#article db id)
         (#f
          `(,Reply#404 ,state ,tx))

         ((? (cut send-challenge? <> identity expected-identity))
          `(,Reply#challenge ,state ,tx))

         (article
          (match type
            (#:html
             `(,(Reply#mk
                 (build-response #:code 200 #:headers (Header#for #:html))
                 (Html#bvector (Article#html article)))
               ,state
               ,tx))
            
            (#:org
             `(,(Reply#mk
                 (build-response #:code 200 #:headers (Header#for #:text))
                 (Text#bvector (Article#org article)))
               ,state
               ,tx))
            
            (_
             `(,Reply#404 ,state ,tx))))))

      (`(#:resource ,identity ,id ,res-id)
       (identity-check identity)
       (String#check id)
       (match (Db#article db id)
         (#f
          `(,Reply#404 ,state ,tx))

         ((? (cut send-challenge? <> identity expected-identity))
          `(,Reply#challenge ,state ,tx))

         (article
          (match (Article#data article res-id)
            (#f `(,Reply#404 ,state ,tx))
            (data
             `(,(Reply#mk
                 (build-response #:code 200 #:headers (data->headers data))
                 (Data#bvector data))
               ,state ,tx))))))      

      (_
       (raise-exception (format #f "Unexpected message. message = ~a" message))))))

(define mk (Actor#mk init tx))

(define (Website#mk env content layout login password)
  (mk `(,env ,content ,layout ,login ,password)))

(define (Website#hello website)
  (Actor#send website #:hello))

(define (Website#404 website)
  (Actor#send website #:x404))

(define (Website#article website identity id type)
  (Actor#send website `(#:article ,identity ,id ,type)))

(define (Website#index website)
  (Actor#send website #:index))

(define (Website#resource website identity id res-id)
  (Actor#send website `(#:resource ,identity ,id ,res-id)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Website#mk
        Website#hello
        Website#article
        Website#resource
        Website#index
        Website#404)
