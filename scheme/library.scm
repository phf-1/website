;;;;;;;;;;;;;;;;;;;
;; Specification ;;
;;;;;;;;;;;;;;;;;;;
;;
;; [[id:f4b947d7-9ed1-4d43-9841-f041c761d58a]]
;;
;; Given Publication, then Library<Publication> :≡ Library#mk(Publication) allows to
;; build collections of publications called libraries. Given dir : LibraryDir, then
;; library :≡ Library<Publication>#mk(dir) represents the collections of
;; publications encoded in dir. Notably, library adds an index of publictions.

;;;;;;;;;;;;;;;;;;;;
;; Implementation ;;
;;;;;;;;;;;;;;;;;;;;

(define-module (library)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 futures)
  #:use-module (publication)
  #:use-module (maybe)
  #:use-module (srfi srfi-19)
  #:export (Library))

(define index-uuid "00000000-0000-0000-0000-000000000001")

(define (publication->href pub)
  (string-append "- [[href:/" (pub #:uuid) "/html][" (pub #:name) "]]"))

(define (generate-index-text publications)
  "Generate org-mode text for the index publication listing all publications"
  (string-append
   "#+ID: " index-uuid "\n"
   "#+TITLE: Articles\n"
   "\n"
   "* Articles\n"
   "\n"
   (string-join (map publication->href publications) "\n")
   "\n"))

(define home-uuid "00000000-0000-0000-0000-000000000000")

(define (Library msg)
  (match msg
    (`(#:mk ,Publication)
     (letrec ((Library<Publication>
               (lambda (msg)
                 (match msg
                   (#:home-uuid home-uuid)

                   (#:index-uuid index-uuid)

                   ;; publications : List(Publication)
                   (`(#:mk ,publications #:List #:Publication)

                    ;; id->pub : Uuid → Publication
                    (let ((id->pub (make-hash-table)))
                      (for-each
                       (lambda (pub) (hash-set! id->pub (pub #:uuid) pub))
                       publications)

                      ;; build the index from publications and add it to id->pub
                      (let ((home-pub (hash-ref id->pub home-uuid)))
                        (unless home-pub (error (format #f "Home publication not found. uuid = ~a" home-uuid)))
                        (let* ((index-text (generate-index-text publications))
                               (delta `((#:text . ,index-text) (#:data . ,(make-hash-table))))
                               (index-pub (Publication `(#:mk ,home-pub ,delta))))
                          (hash-set! id->pub (index-pub #:uuid) index-pub)))

                      ;; library : Library<Publication>
                      (letrec ((library
                                   (lambda (msg)
                                     (match msg
                                       (`(#:publication ,uuid)
                                        (let ((pub (hash-ref id->pub uuid)))
                                          (if pub (just pub) nothing)))

                                       (`(#:html ,uuid)
                                        (let ((maybe-pub (library `(#:publication ,uuid))))
                                          ((maybe-use nothing (lambda (pub) (just (pub #:html)))) maybe-pub)))

                                       (`(#:text ,uuid)
                                        (let ((maybe-pub (library `(#:publication ,uuid))))
                                          ((maybe-use nothing (lambda (pub) (just (pub #:text)))) maybe-pub)))

                                       (`(#:data ,uuid ,id)
                                        (let ((maybe-pub (library `(#:publication ,uuid))))
                                          ((maybe-use nothing (lambda (pub) (pub `(#:data ,id)))) maybe-pub)))

                                       (_ (error (format #f "Unexpected msg: ~a" msg)))))))
                        library)))

                   (`(#:mk ,dir #:Directory)
                    (let* ((entries (filter (lambda (entry) (not (member entry '("." ".."))))
                                            (scandir dir)))
                           (futures (map
                                     (lambda (entry)
                                       (future (Publication `(#:mk ,(string-append dir "/" entry)))))
                                     entries))
                           (publications (map touch futures)))
                      (Library<Publication> `(#:mk ,publications #:List #:Publication))))

                   (_ (error (format #f "Unexpected message: ~a" msg)))))))
       Library<Publication>))
    (_ (error (format #f "Unexpected message: ~a" msg)))))
