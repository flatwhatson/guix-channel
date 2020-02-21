(define-module (flat packages sdl)
  #:use-module (guix packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages sdl))

(define-public guile3.0-sdl2
  (package
   (inherit guile-sdl2)
   (name "guile3.0-sdl2")
   (native-inputs
    (assoc-set!
     (package-native-inputs guile-sdl2)
     "guile" `(,guile-next)))))
