(define-module (flat packages emacs)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc))

;; NOTE: This recipe intentionally leaves out guix-specific patches from the
;; built-in emacs recipes, in an attempt to produce a "vanilla" emacs suitable
;; for shipping with "guix pack".
(define-public emacs-native-comp
  (let ((commit "92dc81f85e1b91db04487ccf1b52c0cd3328dfee")
        (revision "0")
        (emacs-version "28.0.50"))
    (package
     (inherit emacs-next)
     (name "emacs-native-comp")
     (version (git-version emacs-version revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git")
             (commit commit)))
       (sha256
        (base32 "1f22bxwq53hhdjlakmqz66y63vix5ybpnc1pk9fpy18wjh871scq"))
       (file-name (git-file-name name version))
       (patches (search-patches "emacs27-exec-path.patch"
                                "emacs-source-date-epoch.patch"))))
     (arguments
      (substitute-keyword-arguments (package-arguments emacs-next)
       ((#:configure-flags flags)
        `(cons* "--with-nativecomp" ,flags))
       ((#:phases phases)
        `(modify-phases %standard-phases
           ;; Remove hard-coded /bin/pwd from the Makefiles.
           (add-before 'configure 'fix-/bin/pwd
             (lambda _
               (substitute* (find-files "." "^Makefile\\.in$")
                            (("/bin/pwd") "pwd"))
               #t))
           ;; Make the gzip files writable so guix can set timestamps.
           (add-before 'reset-gzip-timestamps 'make-compressed-files-writable
             (lambda _
               (for-each make-file-writable
                         (find-files %output ".*\\.t?gz$"))
               #t))
           ;; Add build-time library paths for libgccjit.
           (add-before 'configure 'set-libgccjit-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((libgccjit-libdir
                      (string-append (assoc-ref inputs "libgccjit")
                                     "/lib/gcc/" %host-type "/"
                                     ,(package-version libgccjit) "/")))
                 (setenv "LIBRARY_PATH"
                         (string-append libgccjit-libdir ":"
                                        (getenv "LIBRARY_PATH"))))
               #t))
           ;; Add runtime library paths for libgccjit,
           (add-after 'glib-or-gtk-wrap 'wrap-library-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((gcc-libdir
                       (string-append (assoc-ref inputs "gcc")
                                      "/lib/"))
                      (glibc-libdir
                       (string-append (assoc-ref inputs "glibc")
                                      "/lib/"))
                      (libgccjit-libdir
                       (string-append (assoc-ref inputs "libgccjit")
                                      "/lib/gcc/" %host-type "/"
                                      ,(package-version libgccjit) "/"))
                      (library-path (list gcc-libdir
                                          glibc-libdir
                                          libgccjit-libdir))
                      (output   (assoc-ref outputs "out"))
                      (bindir   (string-append output "/bin"))
                      (libexec  (string-append output "/libexec"))
                      (bin-list (append (find-files bindir ".*")
                                        (find-files libexec ".*"))))
                 (for-each (lambda (program)
                             (unless (wrapper? program)
                               (wrap-program
                                program
                                `("LIBRARY_PATH" prefix ,library-path))))
                           bin-list))
               #t))
           ;; Emacs' dumper files are incorrectly detected as executables, and
           ;; get wrapped in a launcher script.  Move the originals back.
           (add-after 'wrap-library-path 'restore-emacs-pdmp
             (lambda* (#:key outputs target #:allow-other-keys)
               (let* ((output    (assoc-ref outputs "out"))
                      (libexec   (string-append output "/libexec"))
                      (pdmp      (find-files libexec "^emacs\\.pdmp$"))
                      (pdmp-real (find-files libexec "^\\.emacs\\.pdmp-real$")))
                 (for-each (lambda (wrapper real)
                             (delete-file wrapper)
                             (rename-file real wrapper))
                           pdmp pdmp-real))
               #t))))))
     (inputs
      `(("gcc" ,gcc)
        ("glibc" ,glibc)
        ("libgccjit" ,libgccjit)
        ,@(package-inputs emacs-next))))))
