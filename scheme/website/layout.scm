;; :REF:
;; :ID: 77c09c87-0167-4c92-bcc9-e5eea912c05f

(define-module (website layout))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 textual-ports)
 (ice-9 regex)
 (ice-9 match)
 (rnrs bytevectors)
 (website path)
 (website string)
 (website actor))

(define (embed layout str)
  (string->utf8
   (regexp-substitute/global
    #f "__BODY__" layout 'pre str 'post)))

(define (init dir)
  (Path#directory-check dir)
  (let* ((path (Path#join dir "article.html"))
         (mtime (Path#mtime path))
         (layout (Path#string path)))
    `(,dir ,path ,mtime ,layout)))

(define (tx state msg)
  (match-let ((`(,dir ,path ,mtime ,layout) state))
    (match msg
      (#:update
       (list
        #t
        (list dir path (Path#mtime path) (Path#string path))
        tx))

      (`(#:bvector ,str)
       (String#check str)
       (let ((current-mtime (Path#mtime path)))
         (match (< mtime current-mtime)
           (#f `(,(embed layout str) ,state ,tx))
           (#t (tx (cadr (tx state #:update)) msg)))))

      (_
       (raise-exception (format #f "Unexpected msg. msg = ~a" msg))))))

(define Layout#mk (Actor#mk init tx))

(define (Layout#embed layout str)
  (Actor#send layout `(#:bvector ,str)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Layout#mk
        Layout#embed)
