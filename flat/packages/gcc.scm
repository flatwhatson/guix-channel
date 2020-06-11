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

(define-module (flat packages gcc)
  #:use-module (guix packages)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc))

(define-public libgccjit-10
  (package
    (inherit gcc-10)
    (name "libgccjit")
    (outputs (delete "lib" (package-outputs gcc)))
    (properties (alist-delete 'hidden? (package-properties gcc)))
    (arguments
     (substitute-keyword-arguments `(#:modules ((guix build gnu-build-system)
                                                (guix build utils)
                                                (ice-9 regex)
                                                (srfi srfi-1)
                                                (srfi srfi-26))
                                     ,@(package-arguments gcc))
       ((#:configure-flags flags)
        `(append `("--enable-host-shared"
                   ,(string-append "--enable-languages=jit"))
                 (remove (cut string-match "--enable-languages.*" <>)
                         ,flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'remove-broken-or-conflicting-files
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each delete-file
                         (find-files (string-append (assoc-ref outputs "out") "/bin")
                                     ".*(c\\+\\+|cpp|g\\+\\+|gcov|gcc|gcc-.*)"))
               #t))))))))
