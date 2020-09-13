;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Andrew Whatson <whatson@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (flat packages emacs)
  #:use-module (guix packages)
  #:use-module (guix memoization)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (flat packages gcc))

(define emacs-with-native-comp
  (mlambda (emacs gcc)
    (let ((libgccjit (libgccjit-for-gcc gcc)))
      (package
        (inherit emacs)
        (arguments
         (substitute-keyword-arguments (package-arguments emacs)
           ((#:make-flags flags ''())
            `(cons* "NATIVE_FULL_AOT=1" ,flags))
           ((#:configure-flags flags)
            `(cons* "--with-nativecomp" ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
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
               ;; Add runtime library paths for libgccjit.
               (add-after 'unpack 'patch-driver-options
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "lisp/emacs-lisp/comp.el"
                     (("\\(defcustom comp-native-driver-options nil")
                      (format
                       #f "(defcustom comp-native-driver-options '(~s ~s ~s)"
                       (string-append
                        "-B" (assoc-ref inputs "glibc") "/lib/")
                       (string-append
                        "-B" (assoc-ref inputs "libgccjit") "/lib/")
                       (string-append
                        "-B" (assoc-ref inputs "libgccjit") "/lib/gcc/"))))
                   #t))
               ;; Remove wrappers around .eln files in libexec.
               (add-after 'restore-emacs-pdmp 'unwrap-eln-files
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((output   (assoc-ref outputs "out"))
                          (libexec  (string-append output "/libexec"))
                          (eln-list (find-files libexec "\\.eln$")))
                     (for-each (lambda (wrapper)
                                 (let ((real (string-append
                                              (dirname wrapper) "/."
                                              (basename wrapper) "-real")))
                                   (delete-file wrapper)
                                   (rename-file real wrapper)))
                               eln-list)
                     #t)))))))
        (native-inputs
         `(("gcc" ,gcc)
           ,@(package-native-inputs emacs)))
        (inputs
         `(("glibc" ,glibc)
           ("libgccjit" ,libgccjit)
           ,@(package-inputs emacs)))))))

(define emacs-with-xwidgets
  (mlambda (emacs)
    (package
      (inherit emacs)
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:configure-flags flags)
          `(cons* "--with-xwidgets" ,flags))))
      (inputs
       `(("glib-networking" ,glib-networking)
         ("webkitgtk" ,webkitgtk)
         ,@(package-inputs emacs))))))

(define emacs-with-pgtk
  (mlambda (emacs)
    (package
      (inherit emacs)
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:configure-flags flags)
          `(cons* "--with-pgtk" ,flags)))))))

(define emacs-from-git
  (lambda* (emacs #:key pkg-name pkg-version pkg-revision git-repo git-commit checksum)
    (package
      (inherit emacs)
      (name pkg-name)
      (version (git-version pkg-version pkg-revision git-commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url git-repo)
               (commit git-commit)))
         (sha256 (base32 checksum))
         (file-name (git-file-name pkg-name pkg-version))
         (patches (origin-patches (package-source emacs)))
         (modules (origin-modules (package-source emacs)))
         (snippet (origin-snippet (package-source emacs)))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; Fix strip-double-wrap referencing the wrong version.
             (replace 'strip-double-wrap
               (lambda* (#:key outputs #:allow-other-keys)
                 (with-directory-excursion (assoc-ref outputs "out")
                   (copy-file (string-append "bin/emacs-" ,pkg-version)
                              "bin/emacs")
                   #t)))))))
      (inputs
       `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
         ,@(package-inputs emacs)))
      (native-search-paths
       (list (search-path-specification
              (variable "EMACSLOADPATH")
              (files
               (list "share/emacs/site-lisp"
                     (string-append "share/emacs/" pkg-version "/lisp"))))
             (search-path-specification
              (variable "INFOPATH")
              (files '("share/info"))))))))

(define-public emacs-native-comp
  (emacs-from-git
   (emacs-with-native-comp emacs-next gcc-10)
   #:pkg-name "emacs-native-comp"
   #:pkg-version "28.0.50"
   #:pkg-revision "0"
   #:git-repo "https://git.savannah.gnu.org/git/emacs.git"
   #:git-commit "21021e56ad609a459ec117bcfc60b2802176a9a7"
   #:checksum "0h7s9i5gkdgyw0fwkm4n6zf912hw7i8z03mvka71xi8qlybjsskd"))

(define-public emacs-pgtk-native-comp
  (emacs-from-git
   (emacs-with-pgtk
    (emacs-with-xwidgets
     (emacs-with-native-comp emacs-next gcc-10)))
   #:pkg-name "emacs-pgtk-native-comp"
   #:pkg-version "28.0.50"
   #:pkg-revision "0"
   #:git-repo "https://github.com/flatwhatson/emacs.git"
   #:git-commit "1f3425afba74d1545a4f17fa7bf41d3d54f9f47e"
   #:checksum "1n0skx8sawycd8bca9310725wl53ym9dd3mbcb6xwql4vxd7dy0r"))

(define-public emacs-pgtk-native-comp-dev
  (emacs-from-git
   (emacs-with-pgtk
    (emacs-with-xwidgets
     (emacs-with-native-comp emacs-next gcc-10)))
   #:pkg-name "emacs-pgtk-native-comp-dev"
   #:pkg-version "28.0.50"
   #:pkg-revision "0"
   #:git-repo "https://github.com/flatwhatson/emacs.git"
   #:git-commit "42da227ea08232913aecab2d6aad1b81cab28b6b"
   #:checksum "1dcg2dkh9bbh23pykx5krrkcr1g09cnyzjfdacch5ibik24bj6p3"))
