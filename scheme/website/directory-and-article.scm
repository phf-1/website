;; :REF:
;; :ID: da9eb75e-62de-458e-a5c7-e596245aa03b

(define-module (website directory-and-article))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website article)
             (website html)
             (website text)
             (website layout)
             (website jpeg)
             (sxml simple)
             (website pdf)
             (ice-9 textual-ports)
             (srfi srfi-13)
             (ice-9 peg)
             (rnrs bytevectors)
             (ice-9 binary-ports)
             (ice-9 ftw))

(define-peg-pattern value-peg body (* (and (not-followed-by "\n") peg-any)))
(define-peg-pattern prefix-peg none "#+TITLE:")
(define-peg-pattern ws none (* " "))
(define-peg-pattern title-peg body (and prefix-peg ws value-peg))

(define (bv->title bv)
  (match (search-for-pattern title-peg (utf8->string bv))
    (#f (raise-exception (format #f "No title was found.")))
    (peg (string-trim-both (if (eq? (peg:tree peg) '()) "" (peg:tree peg))))))


(define (title str)
  (if (string= str "")
      ""
      (with-output-to-string
        (lambda () (sxml->xml `(h1 ,str))))))

(define (directory->article dir)
  (Path#directory-check dir)
  (let ((components (Path#components dir))
        (second-to-last #f))
    (unless (> (length components) 1)
      (raise-exception (format #f "not enough components. components = ~a" components)))
    (set! second-to-last (list-ref (reverse components) 1))
    (unless (member second-to-last '("private" "public"))
      (raise-exception (format #f "second-to-last is unexpected. second-to-last = ~a" second-to-last)))
    (let* ((id (basename dir))
           (html-path (Path#join dir "article.html"))
           (org-path (Path#join dir "article.org"))
           (data-path (Path#join dir "data"))
           (private? (string= second-to-last "private"))
           (org (Text#mk "article.org" (call-with-input-file org-path get-bytevector-all)))
           (title-str (bv->title (Text#bvector org)))
           (html (lambda () (Html#mk "article.html" (Layout#bvector (string-append (title title-str) (call-with-input-file html-path get-string-all))))))
           (datas (if (file-exists? data-path)
                      (filter-map
                       (lambda (filename)
                         (let* ((file-path (Path#join data-path filename))
                                (ext (Path#extension filename))
                                (file-id (basename filename)))
                           (and (not (string-prefix? "." filename))
                                (cond
                                 ((string=? ext "jpeg") (Jpeg#mk file-id (call-with-input-file file-path get-bytevector-all)))
                                 ((string=? ext "html") (Html#mk file-id (call-with-input-file file-path get-bytevector-all)))
                                 ((string=? ext "txt") (Text#mk file-id (call-with-input-file file-path get-bytevector-all)))
                                 ((string=? ext "pdf") (Pdf#mk file-id (call-with-input-file file-path get-bytevector-all)))
                                 (else (raise-exception (format #f "Unexpected extension. extension = ~a" ext)))))))
                       (scandir data-path))
                      '())))
      (Article#mk private? id title-str html org datas))))


(define (dir->articles content)
  (Path#directory-check content)
  (let ((entries (scandir content)))
    (filter-map
     (lambda (entry)
       (let ((entry-path (Path#join content entry)))
         (and (file-is-directory? entry-path)
              (not (string-prefix? "." entry))
              (directory->article entry-path))))
     entries)))

(define (contentdirectory->articles dir)
  (Path#directory-check dir)
  (let* ((articles (append
                    (dir->articles (Path#join dir "private"))
                    (dir->articles (Path#join dir "public"))))
         (content (make-hash-table (length articles))))
    (for-each
     (lambda (article)
       (hash-set! content (Article#id article) article))
     articles)
    content))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export directory->article
        contentdirectory->articles)
