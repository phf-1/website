;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF:
;; :ID: 17127fb9-e54d-4155-8a6e-1fb7444ab9ea

(define-module (website login))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website env))

(define (Login#read env)
  (Env#check env)
  (match (getenv "WEBSITE_LOGIN")
    (#f
     (if (Env#dev? env)
         "phf"
         (raise-exception (format #f "login unspecified and env ≠ dev. env = ~a" env))))

    (login login)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Login#read)
