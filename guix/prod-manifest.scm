;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(define prod-manifest
  (specifications->manifest
   '("guile"
     "guile-json"
     "guile-gcrypt"
     "guile-sqlite3"
     "guile-uuid"
     "bash-minimal"
     "make"
     "fd"
     "coreutils-minimal"
     "emacs"
     "emacs-citeproc"
     "emacs-htmlize")))

prod-manifest
