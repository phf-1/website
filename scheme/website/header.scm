;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


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
    (#:pdf  '((content-type . (application/pdf))))
    (#:auth '((www-authenticate . ((basic (realm . "private"))))
              (content-type . (text/plain (charset . "utf-8")))))
    (#:js   '((content-type . (application/javascript (charset . "utf-8")))))
    (#:css  '((content-type . (text/css (charset . "utf-8")))))
    (_ (raise-exception
        (make-exception
         (make-format-exception)
         (make-irritants-condition (list type))))
       (format #f "Unexpected header type: ~a" type))))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Header#for)
