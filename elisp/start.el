;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


;; [[id:e6d438cd-85c0-40d1-a2db-3101fdbe2c12]]
;;
;;   Given that this file is evaluated by Emacs, then executing (start) makes Emacs
;;   wait for content from stdin. After receiving org formatted text data, then HTML
;;   is sent to stdout.

(require 'org)
(require 'ox)
(require 'oc)

;; For some reason, this has to be defined or there is an error.
(defun citeproc-style-cite-superscript-p (x) t)

(defun start--insert-date (date)
  (save-excursion
    (goto-char (point-min))
    (insert (format "<span id=\"last-edit\">Last edit: %s</span>" date))))

(defun start--insert-status (status)
  (save-excursion
    (goto-char (point-min))
    (insert (format "<span id=\"status\">Status: %s</span>" status))))

(defun start--insert-license (url name)
  (save-excursion
    (goto-char (point-min))
    (insert (format "<span id=\"license\">License: <a href=\"%s\">%s</a></span>" url name))))

(defun start--insert-title (title)
  (save-excursion
    (goto-char (point-min))
    (insert (format "<h1>%s</h1>" title))))

(defun start--insert-metadata (metadata)
  (pcase-dolist (`(,key . ,value) metadata)
    (pcase key
      ("DATE"
       (pcase value
         (`(,date)
          (start--insert-date date))
         (_
          (error "ERROR | Unexpected value associated with the %s keyword. value = %s" key value ))))

      ("STATUS"
       (pcase value
         (`(,status)
          (start--insert-status status))
         (_
          (error "ERROR | Unexpected value associated with the %s keyword. value = %s" key value ))))

      ("LICENSE"
       (pcase (string-split (car value) " ")
         (`(,url ,name)
          (start--insert-license url name))
         (_
          (error "ERROR | Unexpected value associated with the %s keyword. value = %s. file = %s" key value (org-get-title)))))

      ("TITLE"
       (pcase value
         (`(,title)
          (start--insert-title title))
         (_
          (error "ERROR | Unexpected value associated with the %s keyword. value = %s" key value))))

      (_
       (message "WARNING | Unexpected keyword. keyword = %s" key)))))

;; Export the current buffer to HTML and switch to the buffer that contains the HTML.
(defun start--export ()
  (let ((org-export-show-temporary-export-buffer t)
        (org-export-with-title t)
        (org-export-with-sub-superscripts nil)
        (org-html-doctype "html5")
        (org-html-html5-fancy t)
        (org-export-with-section-numbers nil)
        (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "DOING(o)" "|" "DONE(d)" "FAILED(f)" "CANCELED(c)")))
        (org-html-head-include-default-style nil)
        (org-html-head-include-scripts nil)
        (org-html-head "@CSS@")
        (org-html-postamble "@JS@")
        (org-cite-export-processors '((t csl)))
        (metadata (org-collect-keywords '("DATE" "LICENSE" "TITLE" "STATUS"))))

    (org-add-link-type "href"
                       :export
                       (lambda (path desc backend)
                         (when (eq backend 'html)
                           (format "<a href=\"%s\">%s</a>" path (or desc path)))))

    (org-html-export-as-html nil nil nil t)
    (start--insert-metadata metadata)))

(defun org-get-date ()
  (let ((date-entry (assoc "DATE" (org-collect-keywords '("DATE")))))
    (when date-entry
      (car (cdr date-entry)))))

(defun start ()
  (with-temp-buffer
    (let ((stdin-content ""))
      (condition-case nil
          (while t
            (setq stdin-content (concat stdin-content (read-from-minibuffer "") "\n")))
        (error nil))
      (insert stdin-content))
    (org-mode)
    (start--export)
    (princ (buffer-string))))

