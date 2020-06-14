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

(define-module (flat packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-flycheck-guile
  (package
    (name "emacs-flycheck-guile")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/flatwhatson/flycheck-guile")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fi6scbfid4pfg0v2zalgwisz8m9hpypl8ssnn0dbyw7lprbld43"))))
    (propagated-inputs
     `(("emacs-flycheck" ,emacs-flycheck)
       ("emacs-geiser" ,emacs-geiser)))
    (build-system emacs-build-system)
    (home-page "https://github.com/flatwhatson/flycheck-guile")
    (synopsis "GNU Guile support for Flycheck")
    (description "This package provides a Flycheck checker for GNU Guile using
@code{guild compile}.")
    (license license:gpl3+)))
