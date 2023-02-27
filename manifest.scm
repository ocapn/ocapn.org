(use-modules (guix)
             (gnu packages guile-xyz))

(packages->manifest
 (specifications->packages
  (list "glibc-locales" "nss-certs" "git" "coreutils" "make" "haunt" "rsync")))
