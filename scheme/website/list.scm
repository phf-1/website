;; :REF:
;; :ID: 8f932d03-d3ed-4bd2-a702-18eddee1588c

(define-module (website list))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
 (srfi srfi-1))

(define* (List#? lst #:optional (pred (lambda (_x) #t)))
  (and (list? lst) (every pred lst)))

(define* (List#check lst #:optional (pred (lambda (_x) #t)))
  (unless (List#? lst pred)
    (raise-exception
     (format #f "lst is not a list of elements that satisfy pred. lst = ~a" lst))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export List#check
 List#?)
