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

(define (article->li article)
  (let ((link (article->link article))
        (private '(code (@ (class "privacy")) "(private)"))
        (public '(code (@ (class "privacy")) "(public)")))
    (li
     (list
      (article->link article)
      (if (Article#private? article) private public)))))

(define (set-of-article->index-html articles)
  (Html#mk "article.html"
           (Layout#bvector
            (sxml->string
             (list
              (h2 '(@ (style "margin-top:0;")) "List of articles")
              (ul (hash-map->list (lambda (_key article) (article->li article)) articles)))))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export set-of-article->index-html)
