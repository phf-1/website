(load "prod-manifest.scm")

(define dev-manifest
  (concatenate-manifests
   (list prod-manifest
         (specifications->manifest
          '("coreutils-minimal"
            "git"
            "tar"
            "zstd"
            "gawk"
            "tree"
            "fd"
            "sed"
            "openssh-sans-x")))))

dev-manifest
