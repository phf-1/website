;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(load "prod-manifest.scm")

(define dev-manifest
  (concatenate-manifests
   (list prod-manifest
         (specifications->manifest
          '("git"
            "tar"
            "zstd"
            "gawk"
            "tree"
            "sed"
            "inotify-tools"
            "openssh-sans-x")))))

dev-manifest
