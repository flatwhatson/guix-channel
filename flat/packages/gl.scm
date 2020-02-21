(define-module (flat packages gl)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages texinfo))

(define-public guile3.0-opengl
  (package
   (inherit guile-opengl)
   (name "guile3.0-opengl")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://git.savannah.gnu.org/git/guile-opengl.git")
                  (commit "687e96b9dfe1e04e9cf4342921fcd751e162a792")))
            (sha256
             (base32 "06l2ibx09pk8ngqvnw9r4gv8hq2lgl0xadbqnpm6zbwddkckhiq4"))))
   (arguments
    (append
     '(#:make-flags '("GUILE_AUTO_COMPILE=0"))
     (package-arguments guile-opengl)))
   (inputs
    (assoc-set!
     (package-inputs guile-opengl)
     "guile" `(,guile-next)))
   (native-inputs
    (append
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("makeinfo" ,texinfo))
     (package-native-inputs guile-opengl)))))
