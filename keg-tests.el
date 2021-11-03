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

(defsubst keg-tests--string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  (let ((i (string-match-p (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
                           string)))
    (if i (substring string 0 i) string)))

(defmacro cort-deftest-with-shell-command (name form)
  "Return `cort-deftest' compare with `string=' for NAME, FORM.

  (cort-deftest-with-shell-command keg/subcommand-help
    '((\"keg version\"
       \"Keg 0.0.1 running on Emacs 26.3\")
      (\"keg files\"
       \"keg-ansi.el\\nkeg-mode.el\\nkeg.el\")))

  => (cort-deftest keg/subcommand-help
       '((:string= \"Keg 0.0.1 running on Emacs 26.3\"
                   (string-trim-right
                    (shell-command-to-string \"keg version\")))
         (:string= \"keg-ansi.el\\nkeg-mode.el\\nkeg.el\"
                   (string-trim-right
                    (shell-command-to-string \"keg files\")))))"
(declare (indent 1))
  `(cort-deftest ,name
     ',(apply #'nconc
              (mapcar (lambda (elm)
                        (mapcar
                         (lambda (regexp)
                           `(:string-match-p
                             ,regexp
                             (keg-tests--string-trim-right (shell-command-to-string ,(car elm)))))
                         (cdr elm)))
                      (cadr form)))))

(cort-deftest-with-shell-command keg/subcommand-simple
  '(("./bin/keg version"
     "Keg [0-9.]+ running on Emacs [0-9.]+")
    ("./bin/keg help"
     "USAGE: keg \\[SUBCOMMAND\\] \\[OPTIONS\\.\\.\\.\\]$"
     "^ build \\[PACKAGE\\]$"
     "^ clean$"
     "^ clean-elc \\[PACKAGE\\]$"
     "^ debug$"
     "^ files \\[PACKAGE\\]$"
     "^ emacs \\[ARGS\\.\\.\\.\\]$"
     "^ eval \\[SEXP\\]$"
     "^ exec COMMAND \\[ARGS\\.\\.\\.\\]$"
     "^ files \\[PACKAGE\\]$"
     "^ help$"
     "^ info \\[PACKAGE\\]$"
     "^ init$"
     "^ install \\[PACKAGES\\.\\.\\.\\]$"
     "^ lint \\[PACKAGE\\]$"
     "^ `?load-path'?$"                 ; Symbol is automatically quoted on Emacs 24.5>=.
     "^ version$")))

(cort-deftest-generate keg/ansi-cl-macrolet :macroexpand
  '(
    ;; With 1 argument
    ((keg-ansi--cl-macrolet
         ((macro-1-arg (arg1) `(let ((x 1)) ,arg1)))
       (macro-1-arg x))
     (let ((x 1)) x))
    ((keg-ansi--cl-macrolet
         ((macro-1-arg (arg1) (message "Expand") `(let ((x 1)) ,arg1)))
       (macro-1-arg x))
     (let ((x 1)) x))

    ;;With &rest arguments
    ((keg-ansi--cl-macrolet
         ((macro-&rest-args (arg1 &rest args) `(let ((x 1)) ,arg1 ,@args)))
       (macro-&rest-args x 1 (2 3) 4))
     (let ((x 1)) x 1 (2 3) 4))
    ((keg-ansi--cl-macrolet
         ((macro-&rest-args (arg1 &rest args) (message "Expand") `(let ((x 1)) ,arg1 ,@args)))
       (macro-&rest-args x 1 (2 3) 4))
     (let ((x 1)) x 1 (2 3) 4))
    ((keg-ansi--cl-macrolet
         ((macro-&rest-args (arg1 &rest args) `(let ((x 1)) ,arg1 ,@args)))
       (macro-&rest-args x [1] (2 3) 4))
     (let ((x 1)) x [1] (2 3) 4))
    ((keg-ansi--cl-macrolet
         ((macro-&rest-args (arg1 &rest args) (message "Expand") `(let ((x 1)) ,arg1 ,@args)))
       (macro-&rest-args x [1] (2 3) 4))
     (let ((x 1)) x [1] (2 3) 4))

    ;; With 1 &optional argument
    ((keg-ansi--cl-macrolet
         ((macro-1-optional-arg (arg1 &optional arg2) `(let ((x 1)) ,arg1 ,arg2)))
       (macro-1-optional-arg x))
     (let ((x 1)) x nil))
    ((keg-ansi--cl-macrolet
         ((macro-1-optional-arg (arg1 &optional arg2) `(let ((x 1)) ,arg1 ,arg2)))
       (macro-1-optional-arg x 1))
     (let ((x 1)) x 1))

    ;; With 2 &optional arguments
    ((keg-ansi--cl-macrolet
         ((macro-2-optional-args (arg1 &optional arg2 arg3) `(let ((x 1)) ,arg1 ,arg2 ,arg3)))
       (macro-2-optional-args x))
     (let ((x 1)) x nil nil))
    ((keg-ansi--cl-macrolet
         ((macro-2-optional-args (arg1 &optional arg2 arg3) `(let ((x 1)) ,arg1 ,arg2 ,arg3)))
       (macro-2-optional-args x 2))
     (let ((x 1)) x 2 nil))
    ((keg-ansi--cl-macrolet
         ((macro-2-optional-args (arg1 &optional arg2 arg3) `(let ((x 1)) ,arg1 ,arg2 ,arg3)))
       (macro-2-optional-args x 2 3))
     (let ((x 1)) x 2 3))

    ;; With multiple bindings
    ((keg-ansi--cl-macrolet
         ((macro-1-arg (arg1) `(let ((x 1)) ,arg1))
          (macro-&rest-args (arg1 &rest args) `(let ((x 1)) ,arg1 ,@args)))
       (macro-1-arg x)
       (macro-&rest-args x))
     (keg-ansi--cl-macrolet
         ((macro-1-arg (arg1) `(let ((x 1)) ,arg1)))
       (keg-ansi--cl-macrolet
           ((macro-&rest-args (arg1 &rest args) `(let ((x 1)) ,arg1 ,@args)))
         (macro-1-arg x)
         (macro-&rest-args x))))))

(cort-deftest-generate keg/build--package-archives :equal
  '(((keg-build--package-archives
      '(gnu))
     '(("gnu" . "https://elpa.gnu.org/packages/")))
    ((keg-build--package-archives
      '((gnu . "http://elpa.zilongshanren.com/gnu/")))
     '(("gnu" . "http://elpa.zilongshanren.com/gnu/")))
    ((keg-build--package-archives
      '("http://elpa.zilongshanren.com/gnu/"))
     '(("http://elpa.zilongshanren.com/gnu/" . "http://elpa.zilongshanren.com/gnu/")))
    ((keg-build--package-archives
      '((gnu . "http://elpa.zilongshanren.com/gnu/")
        (melpa . "http://elpa.zilongshanren.com/melpa/")))
     '(("gnu" . "http://elpa.zilongshanren.com/gnu/")
       ("melpa" . "http://elpa.zilongshanren.com/melpa/")))))

;; (provide 'keg-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg-tests.el ends here
