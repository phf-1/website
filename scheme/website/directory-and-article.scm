;; :REF:
;; :ID: da9eb75e-62de-458e-a5c7-e596245aa03b

(define-module (website directory-and-article))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules
 (ice-9 binary-ports)
 (ice-9 ftw)
 (ice-9 match)
 (ice-9 peg)
 (ice-9 textual-ports)
 (rnrs bytevectors)
 (srfi srfi-1)
 (srfi srfi-13)
 (srfi srfi-19)
 (sxml simple)
 (website article)
 (website html)
 (website utils)
 (website string)
 (website jpeg)
 (website layout)
 (website path)
 (website pdf)
 (website text))

(define-peg-pattern value-peg body (* (and (not-followed-by "\n") peg-any)))
(define-peg-pattern prefix-peg none "#+TITLE:")
(define-peg-pattern ws none (* (or " " "\t")))
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

(define (epoch->html epoch)
  "Given epoch : Epoch, (epoch->html epoch) is a String that represents a HTML tag of
the epoch using ISO8601 format reduced to YYYY-MM-DD."
  (Nat#check epoch)
  (let* ((tm (localtime epoch))
         (year (+ 1900 (tm:year tm)))
         (month (+ 1 (tm:mon tm)))
         (day (tm:mday tm))
         (date-str (format #f "~4,'0d-~2,'0d-~2,'0d" year month day)))
    (with-output-to-string
      (lambda ()
        (sxml->xml `(span (@ (id "last-edit")) "Last edit: " (time (@ (datetime ,date-str)) ,date-str)))))))

(define (status->html status)
  "Given status : String | #f, (status->html status) is a String that encodes the associated
HTML tag."
  (match status
    (#f "")
    (_
     (String#check status)
     (with-output-to-string
       (lambda ()
         (let ((prefix '(a (@ (href "https://rfc.zeromq.org/spec/44/#26-evolution-of-public-contracts")) "Status")))
           (sxml->xml `(span (@ (id "status")) ,prefix ": " ,(string-trim-both status)))))))))

;; Begin String → Epoch | #f
(define-peg-pattern digit body (range #\0 #\9))
(define-peg-pattern date body
  (and digit digit digit digit "-" digit digit "-" digit digit))
(define-peg-pattern date-line body
  (and ws (ignore "#+DATE:") ws date))

(define (line->epoch line)
  (match (match-pattern date-line line)
    (#f #f)
    (m (time-second (date->time-utc (string->date (peg:tree m) "~Y-~m-~d"))))))

(define (string->epoch string)
  "Given org-txt : String, look for the first line in org-txt such that it matches
this pattern: #+DATE: YYYY-MM-DD. Return the date YYYY-MM-DD as an Epoch, #f
otherwise."
  (let ((lines (string-split string #\newline)))
    (let loop ((ls lines))
      (match ls
        (() #f)
        ((head tail ...) (or (line->epoch head) (loop tail)))))))
;; End

;; Begin String → Status | #f
(define-peg-pattern status-line body
  (and ws (ignore "#+STATUS:") ws value-peg))

(define (line->status line)
  (match (match-pattern status-line line)
    (#f #f)
    (m (peg:tree m))))

(define (string->status string)
  "Given org-txt : String, look for the first line in org-txt such that it matches
this pattern: #+STATUS: value. Return the string value if any or #f."
  (let ((lines (string-split string #\newline)))
    (let loop ((ls lines))
      (match ls
        (() #f)
        ((head tail ...) (or (line->status head) (loop tail)))))))
;; End

(define (directory->article dir layout)
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
           (org-txt (Path#string org-path))
           (last-edit (string->epoch org-txt))
           (status (string->status org-txt))
           (org (Text#mk "article.org" (string->utf8 org-txt)))
           (title-str (bv->title (Text#bvector org)))
           (html
            (lambda ()
              (Html#mk
               "article.html"
               (Layout#embed
                layout
                (string-append
                 (epoch->html last-edit)
                 (status->html status)
                 (title title-str)
                 (Path#string html-path))))))
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
      (Article#mk private? id title-str html org datas last-edit status))))


(define (dir->articles content layout)
  (Path#directory-check content)
  (let ((entries (scandir content)))
    (filter-map
     (lambda (entry)
       (let ((entry-path (Path#join content entry)))
         (and (file-is-directory? entry-path)
              (not (string-prefix? "." entry))
              (directory->article entry-path layout))))
     entries)))

(define (contentdirectory->articles dir layout)
  (Path#directory-check dir)
  (let* ((articles (append
                    (dir->articles (Path#join dir "private") layout)
                    (dir->articles (Path#join dir "public") layout)))
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
