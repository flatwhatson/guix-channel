(define-module (flat packages emacs)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc))

;; NOTE: This recipe intentionally leaves out guix-specific patches from the
;; built-in emacs recipes, in an attempt to produce a "vanilla" emacs suitable
;; for shipping with "guix pack".
(define-public emacs-native-comp
  (let ((commit "92cf4bb8cc3da81f4877a734b9e9089ac4b89e85")
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
        (base32 "02l19bnqq5c8qbd6ijcqpjdbafc9mjgcz6ax19inlyvd0v0r6sa9"))
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
           ;; Add library path needed for libgccjit to work, so we can pass the
           ;; "smoke test" in the configure script.
           (add-before 'configure 'set-libgccjit-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((libgccjit (assoc-ref inputs "libgccjit"))
                     (version ,(package-version libgccjit)))
                 (setenv "LIBRARY_PATH"
                         (string-append
                          libgccjit "/lib/gcc/" %host-type "/" version "/"
                          ":" (getenv "LIBRARY_PATH"))))
               #t))
           ;; Make the gzip files writable so guix can set timestamps.
           (add-before 'reset-gzip-timestamps 'make-compressed-files-writable
             (lambda _
               (for-each make-file-writable
                         (find-files %output ".*\\.t?gz$"))
               #t))
           ;; Emacs' dumper files are incorrectly detected as executables, and
           ;; get wrapped in a launcher script.  Move the originals back.
           (add-after 'glib-or-gtk-wrap 'restore-emacs-pdmp
             (lambda* (#:key outputs target #:allow-other-keys)
               (let* ((libexec (string-append (assoc-ref outputs "out")
                                              "/libexec"))
                      (pdmp (find-files libexec "^emacs\\.pdmp$"))
                      (pdmp-real (find-files libexec "^\\.emacs\\.pdmp-real$")))
                 (for-each (lambda (wrapper real)
                             (delete-file wrapper)
                             (rename-file real wrapper))
                           pdmp pdmp-real))
               #t))))))
     (inputs
      `(("libgccjit" ,libgccjit)
        ,@(package-inputs emacs-next))))))
