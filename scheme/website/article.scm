;; :REF: cd651702-bd90-4ddc-962b-2579cd51d845
;; :ID: fcce7b56-22c9-48f2-92a5-be25c1b28ce9

(define-module (website article))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-19)
             (website actor)
             (website list)
             (website data)
             (website utils)
             (website jpeg)
             (website article-and-html)
             (website pdf)
             (rnrs bytevectors)
             (website string)
             (website html)
             (website text))

(define (valid-data? x)
  (or (Jpeg#? x)
      (Html#? x)
      (Pdf#? x)
      (Text#? x)))

(define (init data)
  (match data
    (`(,private ,id ,title ,html ,org ,datas ,last-edit)
     (unless (boolean? private)
       (raise-exception (format #f "private is not a Boolean. private = ~a" private)))
     (String#check id)
     (String#check title)
     (unless (procedure? html)
       (raise-exception (format #f "html is not a procedure that returns a Html actor. html = ~a" html)))
     (Text#check org)
     (List#check datas valid-data?)
     (Nat#check last-edit)
     (let ((state (make-hash-table 5))
           (resources (make-hash-table (length datas))))
       (hashq-set! state #:private private)
       (hashq-set! state #:id id)
       (hashq-set! state #:last-edit last-edit)
       (hashq-set! state #:html html)
       (hashq-set! state #:org org)
       (for-each (lambda (data) (hash-set! resources (Data#id data) data)) datas)
       (hashq-set! state #:resources resources)
       (hashq-set! state #:title title)
       state))

    (_
     (raise-exception (format #f "Unexpected data. data = ~a" data)))))

(define (tx state message)
  (match-let ((private (hashq-ref state #:private))
              (id (hashq-ref state #:id))
              (html (hashq-ref state #:html))
              (org (hashq-ref state #:org))
              (resources (hashq-ref state #:resources))
              (title (hashq-ref state #:title))
              (last-edit (hashq-ref state #:last-edit)))
    (match message
      (`(#:interface? ,id)
       `(,(memq id '(#:Article)) ,state ,tx))
      (#:private `(,private ,state ,tx))
      (#:title `(,title ,state ,tx))
      (#:id `(,id ,state ,tx))
      (#:last-edit `(,last-edit ,state ,tx))
      (#:html `(,(let ((value (html))) (Html#check value) value) ,state ,tx))
      (#:org `(,org ,state ,tx))
      (`(#:data ,id)
       (String#check id)
       `(,(hash-ref resources id) ,state ,tx))
      (#:repr
       (let ((repr #f))
         (set! repr
               (format #f "Article#mk(private=~a id=~a title=~a html=~a org=~a datas=~a)"
                       private id title (Html#repr (html))
                       (Text#repr org)
                       (hash-map->list (lambda (_key data) (Data#repr data)) resources)))
         `(,repr ,state ,tx)))
      (_
       (raise-exception (format #f "Unexpected message. message = ~a" message))))))

(define mk (Actor#mk init tx))

(define (Article#mk private id title html org datas last-edit)
  (mk `(,private ,id ,title ,html ,org ,datas ,last-edit)))

(define (Article#private? article)
  (Actor#send article #:private))

(define (Article#id article)
  (Actor#send article #:id))

(define (Article#title article)
  (Actor#send article #:title))

(define (Article#repr article)
  (Actor#send article #:repr))

(define (Article#html article)
  (Actor#send article #:html))

(define (Article#last-edit article)
  (Actor#send article #:last-edit))

(define (Article#org article)
  (Actor#send article #:org))

(define (Article#data article id)
  (Actor#send article (list #:data id)))

(define (Article#index articles layout)
  (define html (lambda () (set-of-article->index-html articles layout)))
  (define org (Text#mk "article.org" (string->utf8 "")))
  (Article#mk #f "index" "Index of articles" html org '() (time-second (current-time))))

(define (Article#? x)
  (and (procedure? x)
       (catch #t
         (lambda () (Actor#send x '(#:interface? #:Article)))
         (lambda (key . args) #f))))

(define (Article#check x)
  (unless (Article#? x)
    (raise-exception (format #f "x is not an Article. x = ~a" x))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Article#mk
        Article#mk_from
        Article#?
        Article#check
        Article#index
        Article#private?
        Article#id
        Article#check
        Article#title
        Article#html
        Article#last-edit
        Article#repr
        Article#org
        Article#data)
