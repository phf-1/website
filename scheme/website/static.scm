;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; :REF: 7dcb4d1b-7881-4834-af1b-ce581461a56f
;; :ID: c49ce265-758f-41ee-8c59-e397fc09e124

(define-module (website static))

;;;;;;;;;;;;;
;; Context ;;
;;;;;;;;;;;;;

(use-modules (ice-9 match)
             (srfi srfi-1)
             (website path)
             (website string)
             (website js)
             (website css)
             (website actor)
             (website path)
             (ice-9 ftw))

(define (index-resource dir hash)
  (lambda (name)
    (when (hash-ref hash name)
      (raise-exception (format #f "name ∈ hash. name = ~a" name)))

    (let ((path (Path#join dir name))
          (resource #f))
      (set! resource
            (match (Path#extension name)
              ("js" (Js#mk name (Path#utf8 path)))
              ("css" (Css#mk name (Path#utf8 path)))
              (ext (raise-exception (format #f "unsupported file extension. extension = ~a" ext)))))
      (hash-set! hash name resource))))

(define (init _data)
  (make-hash-table 100))

(define (tx state msg)
  (match-let ((hash state))
    (match msg
      (`(#:index ,dir)
       (Path#directory-check dir)
       (for-each (index-resource dir hash) (filter (lambda (name) (not (string-prefix? "." name))) (scandir dir)))
       `(#:ok ,state ,tx))

      (`(#:get ,name)
       (String#check name)
       `(,(hash-ref hash name) ,state ,tx))

      (_
       (raise-exception (format #f "Unexpected message. msg = ~a" msg))))))

(define mk (Actor#mk init tx))

(define (Static#mk)
  (mk #f))

(define (Static#index static dir)
  (Actor#send static `(#:index ,dir)))

(define (Static#get static name)
  (Actor#send static `(#:get ,name)))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export Static#mk
        Static#index
        Static#get)
