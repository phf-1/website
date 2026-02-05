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
(setq org-export-with-title t)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-export-with-section-numbers nil)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "DOING(o)" "|" "DONE(d)" "FAILED(f)" "CANCELED(c)")))

(defun citeproc-style-cite-superscript-p (x) t)

(org-add-link-type "href" :export
                   (lambda (path desc backend)
                     (when (eq backend 'html)
                       (format "<a href=\"%s\">%s</a>" path (or desc path)))))

(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-html-head "@CSS@")
(setq org-html-postamble "@JS@")
(setq org-export-show-temporary-export-buffer t)

(setq org-cite-export-processors '((t csl)))

(defun start ()
  (with-temp-buffer
    (let ((stdin-content ""))
      (condition-case nil
          (while t
            (setq stdin-content (concat stdin-content (read-from-minibuffer "") "\n")))
        (error nil))
      (insert stdin-content))
    (org-mode)
    (let ((title (org-get-title)))
      (org-html-export-as-html nil nil nil t)
      (when title
        (goto-char (point-min))
        (insert (format "<h1>%s</h1>" title)))
      (princ (buffer-string)))))


;; TODO: print

;; (setq org-export-with-broken-links t)

;; Begin derived html backend
;;
;; Exercise headlines are mapped to <hi exercise …
;;
;; Implementation:
;; (require 'ox)
;; (require 'ox-html)

;; (defun exercise-p (headline)
;;   (let ((type (org-entry-get headline "TYPE" nil))
;;          (id (org-entry-get headline "ID" nil)))
;;     (and (stringp id)
;;          (stringp type)
;;          (string= type "b0d53cd4-ad89-4333-9ef1-4d9e0995a4d8"))))

;; (defun index (parent headline)
;;   (let (contents)
;;     (setq contents (seq-filter (lambda (el) (eq (org-element-type el) 'headline)) (org-element-contents parent)))
;;     (seq-position contents headline #'eq)))

;; (defun question-p (headline)
;;   (let (parent)
;;     (setq parent (org-element-parent headline))
;;     (and (exercise-p parent) (eql (index parent headline) 0))))

;; (defun answer-p (headline)
;;   (let (parent)
;;     (setq parent (org-element-parent headline))
;;     (and (exercise-p parent) (eql (index parent headline) 1))))

;; (defun headline-with-exercise (headline contents info)
;;   (cond
;;    ((exercise-p headline)
;;     (org-element-put-property headline :HTML_CONTAINER_CLASS "exercise")
;;     (org-element-put-property headline :UNNUMBERED "notoc"))
;;    ((question-p headline)
;;     (org-element-put-property headline :HTML_CONTAINER_CLASS "question"))
;;    ((answer-p headline)
;;     (org-element-put-property headline :HTML_CONTAINER_CLASS "answer")))
;;   (org-export-with-backend 'html headline contents info))

;; (org-export-define-derived-backend 'html-with-exercises 'html
;;   :translate-alist '((headline . headline-with-exercise)))
;; ;; End

;; (org-export-to-file 'html-with-exercises
;;     (concat (file-name-sans-extension (buffer-file-name)) ".html")
;;   nil nil nil t)
