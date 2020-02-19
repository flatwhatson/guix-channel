(use-modules (guix git-download)
             (guix packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages gl)
             (gnu packages texinfo))

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
      (package-native-inputs guile-opengl))))
