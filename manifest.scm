(use-modules (guix)
             (gnu packages guile-xyz))

(packages->manifest
 (specifications->packages
  (list "nss-certs" "git" "coreutils" "make" "haunt" "rsync")))
