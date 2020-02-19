(use-modules (guix packages)
             (gnu packages guile)
             (gnu packages sdl))

(package
  (inherit guile-sdl2)
  (name "guile3.0-sdl2")
  (native-inputs
    (assoc-set!
      (package-native-inputs guile-sdl2)
      "guile" `(,guile-next))))
