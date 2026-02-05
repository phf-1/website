;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;;;;;;;;;;;;;;;;;;;
;; Specification ;;
;;;;;;;;;;;;;;;;;;;
;;
;; [[id:c4fa289d-1eed-4bd3-b61a-78159eb36aab]]
;;
;; Given start-el, then parser :≡ Parser#mk(start-el) is an actor such that it
;; maps strings or list of strings into various internal structures. For instance,
;; it maps CLI arguments into messages, Org formatted strings into HTML body
;; content, ….
;;
;; [[ref:e6d438cd-85c0-40d1-a2db-3101fdbe2c12][start-el]]

;;;;;;;;;;;;;;;;;;;;
;; Implementation ;;
;;;;;;;;;;;;;;;;;;;;

(define-module (parser)
  #:use-module (web uri)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:export (Parser))

(define (emacs-export-body text start-el bibliography-bib csl-dir)
  "Run Emacs batch export on TEXT via stdin and return the generated HTML body."
  (let ((command `("emacs" "-Q" "--batch"
                   "--load" ,start-el
                   "--eval" ,(format #f "(setq org-cite-global-bibliography '(\"~a\"))" bibliography-bib)
                   "--eval" ,(format #f "(setq org-cite-csl-styles-dir \"~a\")" csl-dir)
                   "--eval" "(start)")))
    (let-values (((from-emacs to-emacs pids) (pipeline (list command))))
      (display text to-emacs)
      (close-port to-emacs)
      (let ((body (get-string-all from-emacs)))
        (close-port from-emacs)
        (for-each waitpid pids) ;; no zombies
        body))))

(define (Parser msg)
  (match msg
    (`(#:mk ,start-el ,bibliography-bib ,pnas-dir)
     (lambda (msg)
       (match msg
         (`(#:args ,args)
          (match args
            (`(":start" ,port-str ,dir)
             `(#:start ,(string->number port-str) ,dir))
            (_ (error "Unexpected args: ~a" args))))

         (`(#:body ,text)
          (emacs-export-body text start-el bibliography-bib pnas-dir))

         (`(#:uri ,uri-str)
          (split-and-decode-uri-path uri-str))

         (`(#:binary-type ,ext)
          (match ext
            ("png" #:png)
            ((or "jpg" "jpeg") #:jpg)
            ("pdf" #:pdf)
            ("woff2" #:woff2)
            (_ (error (format #f "unexpected extension ~a" ext)))))

         (_
          (error (format #f "Unexpected msg. msg = ~a" msg))))))))
