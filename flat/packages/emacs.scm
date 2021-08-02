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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (flat packages)
  #:use-module (flat packages gcc)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define emacs-with-native-comp
  (lambda* (emacs gcc #:optional full-aot)
    (let ((libgccjit (libgccjit-for-gcc gcc)))
      (package
        (inherit emacs)
        (source
         (origin
           (inherit (package-source emacs))
           (patches
            (append (search-patches "emacs-native-comp-exec-path.patch")
                    (filter
                     (lambda (f)
                       (not (any (cut string-match <> f)
                                 '("/emacs-exec-path\\.patch$"
                                   "/emacs-ignore-empty-xim-styles\\.patch$"))))
                     (origin-patches (package-source emacs)))))))
        (arguments
         (substitute-keyword-arguments (package-arguments emacs)
           ((#:make-flags flags ''())
            (if full-aot
                `(cons* "NATIVE_FULL_AOT=1" ,flags)
                flags))
           ((#:configure-flags flags)
            `(cons* "--with-native-compilation" ,flags))
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
                     (("\\(defcustom native-comp-driver-options nil")
                      (format
                       #f "(defcustom native-comp-driver-options '(~s ~s ~s ~s)"
                       (string-append
                        "-B" (assoc-ref inputs "binutils") "/bin/")
                       (string-append
                        "-B" (assoc-ref inputs "glibc") "/lib/")
                       (string-append
                        "-B" (assoc-ref inputs "libgccjit") "/lib/")
                       (string-append
                        "-B" (assoc-ref inputs "libgccjit") "/lib/gcc/"))))
                   #t))))))
        (native-inputs
         `(("gcc" ,gcc)
           ,@(package-native-inputs emacs)))
        (inputs
         `(("glibc" ,glibc)
           ("libgccjit" ,libgccjit)
           ,@(package-inputs emacs)))))))

(define emacs-from-git
  (lambda* (emacs #:key pkg-name pkg-version pkg-revision git-repo git-commit checksum)
    (package
      (inherit emacs)
      (name pkg-name)
      (version (git-version pkg-version pkg-revision git-commit))
      (source
       (origin
         (inherit (package-source emacs))
         (method git-fetch)
         (uri (git-reference
               (url git-repo)
               (commit git-commit)))
         (sha256 (base32 checksum))
         (file-name (git-file-name pkg-name pkg-version))))
      (outputs
       '("out" "debug")))))

(define-public emacs-native-comp
  (emacs-from-git
   (emacs-with-native-comp emacs-next gcc-10 'full-aot)
   #:pkg-name "emacs-native-comp"
   #:pkg-version "28.0.50"
   #:pkg-revision "180"
   #:git-repo "https://git.savannah.gnu.org/git/emacs.git"
   #:git-commit "e68bc86bcfcc6431c4cf1e8a75b608d76ca1d1ae"
   #:checksum "1gsl0wfjz3pb3jw9arikp7994nknam6dcnrn5sr5k2nay8rm5ing"))

(define-public emacs-pgtk-native-comp
  (emacs-from-git
   (emacs-with-native-comp emacs-next-pgtk gcc-10 'full-aot)
   #:pkg-name "emacs-pgtk-native-comp"
   #:pkg-version "28.0.50"
   #:pkg-revision "202"
   #:git-repo "https://github.com/flatwhatson/emacs.git"
   #:git-commit "22c9b8f27f1c31a502c498d1394a3ee7bd96bb48"
   #:checksum "07gfgm8dag86fc8id2g8sp9cq6kq0h7zch3yhqr7zr706i3qsx5l"))
