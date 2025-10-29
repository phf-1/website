;; :REF:
;; :ID: 265b2a31-567d-4ce4-b01c-673207f5082f

(define-module (website article-and-html))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (sxml simple)
             (website html)
             (website utils)
             (website article)
             (website layout))

(define (link id title)
  `(a (@ (href ,(format #f "/article/~a/html" id))) ,title))

(define (li el)
  `(li ,el))

(define (ul lis)
  `(ul ,@lis))

(define (h2 params content)
  `(h2 ,params ,content))

(define (sxml->string sxml)
  (with-output-to-string
    (lambda ()
      (sxml->xml sxml))))

(define (article->link article)
  (link (Article#id article) (Article#title article)))

(define (epoch->sxml epoch)
  "TODO: remove this duplicated code.
Given epoch : Epoch, (epoch->html epoch) is a String that represents a HTML tag of
the epoch using ISO8601 format reduced to YYYY-MM-DD."
  (Nat#check epoch)
  (let* ((tm (localtime epoch))
         (year (+ 1900 (tm:year tm)))
         (month (+ 1 (tm:mon tm)))
         (day (tm:mday tm))
         (date-str (format #f "~4,'0d-~2,'0d-~2,'0d" year month day)))
    `(span (@ (id "last-edit")) (time (@ (datetime ,date-str)) ,date-str))))

(define (set-of-article->index-html articles layout)
  "Given articles : List(Article), layout : Layout, (set-of-article->index-html articles
layout) is an HTML document that contains a table of rows. Each row contains three
pieces of information: the name of an article, its privacy status (public or private)
and its last edit date. Rows are sorted top to bottom by beginning with the most
recently edited article."

  (define (article->row article)
    (let* ((title (Article#title article))
           (id (Article#id article))
           (private? (Article#private? article))
           (epoch (Article#last-edit article))
           (privacy (if private?
                        '(code (@ (class "privacy private")) "private")
                        '(code (@ (class "privacy public")) "public")))
           (link `(a (@ (href ,(format #f "/article/~a/html" id))) ,title))
           (date-html (epoch->sxml epoch)))
      `(tr
        (td ,link)
        (td ,date-html)
        (td ,privacy))))

  (define sorted-articles
    (sort (hash-map->list (lambda (_id a) a) articles)
          (lambda (a b)
            (> (Article#last-edit a)
               (Article#last-edit b)))))

  (define table
    `(table
      (thead
       (tr
        (th "Title")
        (th "Last edit")
        (th "Privacy")))
      (tbody ,@(map article->row sorted-articles))))

  (Html#mk
   "article.html"
   (Layout#embed
    layout
    (sxml->string
     (list
      (h2 '(@ (style "margin-top:0;")) "List of articles")
      table)))))


;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export set-of-article->index-html)
