;; :REF:
;; :ID: 21fdf918-de53-47e4-bfda-f71aed348c9c

(define-module (website password))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website env))

(define (Password#read env)
  (Env#check env)
  (match (getenv "WEBSITE_PASSWORD")
    (#f
     (if (Env#dev? env)
         "phf"
         (raise-exception
          (format #f "password unspecified and env ≠ dev. env = ~a" env))))
    (password password)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Password#read)
