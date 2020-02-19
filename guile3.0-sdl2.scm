(use-modules (guix packages)
             (gnu packages guile)
             (gnu packages sdl)
             (gnu packages tls))

(define guile-variant-package-name
  (@@ (gnu packages guile) guile-variant-package-name))

(define package-for-guile-3.0
  (package-input-rewriting `((,guile-2.2 . ,guile-next)
                             (,gnutls    . ,guile3.0-gnutls))
                           (guile-variant-package-name "guile3.0")))

(package-for-guile-3.0 guile-sdl2)
