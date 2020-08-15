;;; keg-tests.el --- Test definitions for keg  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/keg.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test definitions for `keg'.


;;; Code:

(require 'cort)
(require 'keg)

;; (cort-deftest keg/subcommand-simple
;;   (cort-generate :shell-command
;;     '(("keg version"
;;        "Keg 0.0.1 running on Emacs 26.3")
;;       ("keg help"
;;        "USAGE: keg [SUBCOMMAND] [OPTIONS...]
;;
;; Modern Elisp package development system.
;;
;; SUBCOMMANDS:
;;  build [PACKAGE]
;;      Byte compile for PACKAGE.
;;  clean
;;      Clean ‘.elc’ files and ‘.keg’ sandbox.
;;  clean-elc [PACKAGE]
;;      Clean ‘.elc’ files.
;;  debug
;;      Show debug information.
;;  files [PACKAGE]
;;      Show Elisp files associated with PACKAGE.
;;  emacs [ARGS...]
;;      Exec Emacs with appropriate environment variables.
;;  eval [SEXP]
;;      Eval SEXP via batch Emacs with appropriate environment variables.
;;  exec COMMAND [ARGS...]
;;      Exec COMMAND with appropriate environment variables.
;;  files [PACKAGE]
;;      Show files associated with PACKAGE.
;;  help
;;      Show this help.
;;  info [PACKAGE]
;;      Show PACKAGE information.
;;  init
;;      Create Keg template file.
;;  install [PACKAGE]
;;      Install PACKAGE dependencies in .keg sandbox folder.
;;  lint [PACKAGE]
;;      Exec linters for PACKAGE.
;;  load-path
;;      Show Emacs appropriate ‘load-path’ same format as PATH.
;;  version
;;      Show ‘keg’ version."))))

(defmacro cort-deftest-with-file-template (name template form)
  "Define test with NAME TEMPLATE and FORM."
  (declare (indent 1))
  (let* ((dir (format "%s.test"
                      (replace-regexp-in-string
                       "/" "--" (symbol-name name))))
         (abspath (expand-file-name dir)))
    `(cort-deftest-generate-with-hook :equal ,name
       (lambda ()
         (mkdir ,dir)
         (let ((default-directory ,abspath))
           ,@(mapcar
              (lambda (elm)
                `(with-temp-file ,(car elm)
                   (insert ,(cdr elm))))
              (eval template))))
       (lambda ()
         (delete-directory ,dir 'recursive))
       ',(mapcar
          (lambda (elm)
            `((let ((default-directory ,abspath))
                ,(car elm))
              ,(cadr elm)))
          (eval form)))))

(cort-deftest-with-file-template keg/keg-file
  '(("Keg" . "\
(source gnu melpa)
(package (keg-file))")
    ("keg-file.el" . ""))
  '(((file-relative-name (keg-file-dir))
     "./")

    ((progn
       (mkdir "test")
       (let ((default-directory (expand-file-name "test")))
         (file-relative-name (keg-file-dir))))
     "../")))

;; (provide 'keg-tests)

;;; keg-tests.el ends here
