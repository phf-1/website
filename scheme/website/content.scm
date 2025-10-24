;; :REF:
;; :ID: 9e838e94-7e7c-499b-a4c0-be14bc069550

(define-module (website content))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path))

(define (Content#read)
  (match (getenv "WEBSITE_CONTENT")
    (#f
     (raise-exception (format #f "content is unspecified.")))

    (content
     (Path#directory-check content)
     content)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Content#read)
