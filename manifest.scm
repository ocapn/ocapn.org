(use-modules (guix)
             (gnu packages guile-xyz))

(packages->manifest
 (specifications->packages
  (list "haunt")))
