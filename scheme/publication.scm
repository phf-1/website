;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;;;;;;;;;;;;;;;;;;;
;; Specification ;;
;;;;;;;;;;;;;;;;;;;
;;
;; [[id:11262bd0-6696-4a20-9164-2191e53d4d2f]]
;;
;; Given parser : [1], then Publication<parser> : Type :≡
;; Publication#mk(parser). Given publication : Publication<parser>, then publication
;; may be viewed as an HTML file (publication#html()). Notably, a publication may be
;; built from a PublicationDir[2] using Publication<parser>#mk(dir).
;;
;; [1]: [[ref:c4fa289d-1eed-4bd3-b61a-78159eb36aab]]
;; [2]: [[ref:4ee84d7f-12d6-4bcf-a00f-a049631063f1]]

;;;;;;;;;;;;;;;;;;;;
;; Implementation ;;
;;;;;;;;;;;;;;;;;;;;

(define-module (publication)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (maybe)
  #:export (Publication))

(define uuid-regexp
  (let ((chars "[0-9a-fA-F]"))
    (make-regexp (format #f "^~a{8}-~a{4}-~a{4}-~a{4}-~a{12}$" chars chars chars chars chars))))

(define (validate-and-canonicalize-uuid str context)
  (let* ((trimmed (string-trim-both str))
         (match (regexp-exec uuid-regexp trimmed)))
    (if match
        (string-downcase trimmed)
        (error (format #f "Invalid UUID format ~s ~a" str context)))))

(define (file-is-directory? path)
  (let ((st (false-if-exception (stat path))))
    (and st (eq? 'directory (stat:type st)))))

(define (file-is-regular? path)
  (let ((st (false-if-exception (stat path))))
    (and st (eq? 'regular (stat:type st)))))

(define (concat-files-in-dir dir)
  "Return concatenated content of all regular files in DIR, sorted alphabetically.
   Returns \"\" if DIR has no regular files."
  (let* ((entries (scandir dir))
         (files (and entries
                     (filter (lambda (name) (not (member name '("." ".."))))
                             entries))))
    (if (or (not files) (null? files))
        ""
        (string-join
         (map (lambda (name)
                (call-with-input-file (string-append dir "/" name)
                  get-string-all))
              (sort files string<?))
         "\n\n"))))

(define (optional-concat dir)
  "Return nothing if DIR missing or empty, else just(concatenated content)."
  (if (file-is-directory? dir)
      (let ((content (concat-files-in-dir dir)))
        (if (string-null? content)
            nothing
            (just content)))
      nothing))

(define (mandatory-subdir-concat dir subdir-name)
  "Return concatenated content of mandatory sub-directory.
   Errors if missing or contains no files."
  (let ((path (string-append dir "/" subdir-name)))
    (if (not (file-is-directory? path))
        (error (format #f "Missing mandatory ~a/ directory in ~a" subdir-name dir)))
    (let ((content (concat-files-in-dir path)))
      (if (string-null? content)
          (error (format #f "Mandatory ~a/ directory exists but contains no files in ~a" subdir-name dir))
          content))))

(define (load-mandatory-text-file dir filename)
  "Load mandatory text file inside publication directory."
  (let ((path (string-append dir "/" filename)))
    (if (file-exists? path)
        (call-with-input-file path get-string-all)
        (error (format #f "Missing mandatory ~a in ~a" filename dir)))))

(define (extract-name-from-text text)
  (let* ((lines (string-split text #\newline))
         (id-line (let loop ((ls lines))
                    (if (null? ls)
                        (error (format #f "No #+TITLE: line found in Org files in"))
                        (let* ((line (car ls))
                               (trimmed-left (string-trim line))
                               (upper (string-upcase trimmed-left)))
                          (if (string-prefix? "#+TITLE:" upper)
                              trimmed-left
                              (loop (cdr ls))))))))
    (let* ((colon-pos (string-index id-line #\:))
           (raw-value (substring id-line (+ colon-pos 1)))
           (value (string-trim-both raw-value)))
      value)))

(define (extract-uuid-from-text text)
  (let* ((lines (string-split text #\newline))
         (id-line (let loop ((ls lines))
                    (if (null? ls)
                        (error (format #f "No #+ID: line found in Org files in"))
                        (let* ((line (car ls))
                               (trimmed-left (string-trim line))
                               (upper (string-upcase trimmed-left)))
                          (if (string-prefix? "#+ID:" upper)
                              trimmed-left
                              (loop (cdr ls))))))))
    (let* ((colon-pos (string-index id-line #\:))
           (raw-value (substring id-line (+ colon-pos 1)))
           (value (string-trim-both raw-value)))
      value)))

(define (validate-and-return-uuid dir text)
  "Validate directory basename and #+ID: are valid UUIDs and match (case-insensitively).
   Returns canonical lowercase UUID."
  (let* ((dir-base (basename dir))
         (dir-uuid (validate-and-canonicalize-uuid dir-base (format #f "in directory name ~a" dir)))
         (file-uuid-raw (extract-uuid-from-text text))
         (file-uuid (validate-and-canonicalize-uuid file-uuid-raw (format #f "in #+ID: in ~a" dir))))
    (if (string=? dir-uuid file-uuid)
        dir-uuid
        (error (format #f "UUID mismatch: directory ~a vs #+ID: ~a in ~a"
                       dir-uuid file-uuid dir)))))

(define (compose-html layout body css-maybe js-maybe)
  "Compose final HTML using layout, lazily generated body, and optional CSS/JS."
  (let* ((html layout)
         (html (regexp-substitute/global #f "@BODY@" html 'pre body 'post))
         (css-repl ((maybe-use "<style></style>"
                               (lambda (s) (string-append "<style>" s "</style>")))
                    css-maybe))
         (html (regexp-substitute/global #f "@CSS@" html 'pre css-repl 'post))
         (js-repl ((maybe-use "<script></script>"
                              (lambda (s) (string-append "<script>" s "</script>")))
                   js-maybe))
         (html (regexp-substitute/global #f "@JS@" html 'pre js-repl 'post)))
    html))

(define (load-data-table dir parser)
  "Load all regular files from data/ into a hash table: filename → (binary . type)."
  (let ((data-dir (string-append dir "/data"))
        (table (make-hash-table)))
    (when (file-is-directory? data-dir)
      (let ((entries (scandir data-dir)))
        (when entries
          (for-each
           (lambda (name)
             (when (and (not (member name '("." "..")))
                        (file-is-regular? (string-append data-dir "/" name)))
               (let* ((path (string-append data-dir "/" name))
                      (ext-match (string-match "\\.([^\\.]+)$" name))
                      (ext (if ext-match
                               (string-downcase (match:substring ext-match 1))
                               ""))
                      (type (parser `(#:binary-type ,ext)))
                      (binary (call-with-input-file path get-bytevector-all)))
                 (hash-set! table name (cons binary type)))))
           entries))))
    table))

(define (Publication msg)
  (match msg
    (`(#:mk ,parser)
     (letrec ((Publication<parser>
               (lambda (msg)
                 (match msg
                   (`(#:mk ,text ,css ,js ,layout ,data)
                    (let* ((uuid (extract-uuid-from-text text))
                           (name (extract-name-from-text text))
                           (body (parser `(#:body ,text)))
                           (html (compose-html layout body css js)))

                      (lambda (msg)
                        (match msg
                          (#:name name)
                          (#:text text)
                          (#:css css)
                          (#:js js)
                          (#:layout layout)
                          (#:data data)
                          (#:uuid uuid)
                          (#:html html)
                          (`(#:data ,filename)
                           (let ((pair (hash-ref data filename #f)))
                             (if pair (just pair) nothing)))
                          (_
                           (error (format #f "Unexpected message: ~a" msg)))))))

                   (`(#:mk ,dir)
                    (let* ((text (mandatory-subdir-concat dir "org"))
                           (_uuid (validate-and-return-uuid dir text))
                           (css (optional-concat (string-append dir "/css")))
                           (js (optional-concat (string-append dir "/js")))
                           (layout (load-mandatory-text-file dir "layout.html"))
                           (data (load-data-table dir parser)))

                      (Publication<parser> `(#:mk ,text ,css ,js ,layout ,data))))

                   (`(#:mk ,pub ,delta)
                    (let ((text (or (assq-ref delta #:text) (pub #:text)))
                          (css (or (assq-ref delta #:css) (pub #:css)))
                          (js (or (assq-ref delta #:js) (pub #:js)))
                          (layout (or (assq-ref delta #:layout) (pub #:layout)))
                          (data (or (assq-ref delta #:data) (pub #:data))))
                      (Publication<parser> `(#:mk ,text ,css ,js ,layout ,data))))

                   (_ (error (format #f "Unexpected message: ~a" msg)))))))
       Publication<parser>))
    (_ (error (format #f "Unexpected message: ~a" msg)))))
