(org-add-link-type "href" :export
                   (lambda (path desc backend)
                     (when (eq backend 'html)
                       (format "<a href=\"%s\">%s</a>" path (or desc path)))))

(setq org-export-with-broken-links t)
(setq org-export-with-title t)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-export-with-section-numbers nil)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "DOING(o)" "|" "DONE(d)" "FAILED(f)" "CANCELED(c)")))

(org-html-export-to-html nil nil nil t)
