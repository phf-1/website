;; :REF:
;; :ID: d6059195-eb6e-4ade-9873-4fde1a1cc132

(define-module (website header))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1))

(define (Header#for type)
  (match type
    (#:text '((content-type . (text/plain (charset . "utf-8")))))
    (#:jpeg '((content-type . (image/jpeg))))
    (#:html '((content-type . (text/html (charset . "utf-8")))))
    (#:pdf '((content-type . (application/pdf))))
    (#:auth '((www-authenticate . ((basic (realm . "private"))))
              (content-type . (text/plain (charset . "utf-8")))))
    (_ (raise-exception (format #f "Unexpected type. type = ~a" type)))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Header#for)
