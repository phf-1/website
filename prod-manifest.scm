(define prod-manifest
  (specifications->manifest
   '("guile"
     "guile-json"
     "guile-gcrypt"
     "guile-sqlite3"
     "guile-uuid"
     "bash-minimal"
     "make"
     "emacs"
     "emacs-citeproc"
     "emacs-htmlize")))

prod-manifest
