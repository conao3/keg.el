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
(require 'cl-lib)
(require 'keg)

(defsubst keg-tests--string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  (let ((i (string-match-p (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
                           string)))
    (if i (substring string 0 i) string)))

(defun shell-command-return-tuple (command)
  "Run COMMAND and return (EXITCODE STDOUT STDERR)."
  (with-temp-buffer
    (let ((stdout (current-buffer)))
      (with-temp-buffer
        (let ((stderr (current-buffer)))
          (list (shell-command command stdout stderr)
                (with-current-buffer stdout (buffer-substring-no-properties (point-min) (point-max)))
                (with-current-buffer stderr (buffer-substring-no-properties (point-min) (point-max)))))))))

(cl-defmacro cort-deftest-with-shell-command (name form &key working-directory)
  "Return `cort-deftest' compare with `string=' for NAME, FORM.
If WORKING-DIRECTORY is non-nil, it should be string which specifies directory,
where the shell command will run.

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
                             (keg-tests--string-trim-right
                              (nth 1 (shell-command-return-tuple ,(car elm))))))
                         (cdr elm)))
                      (cadr form)))))

(defsubst car-equal (cons1 cons2)
  "Return non-nil when car of CONS1 equals car of CONS2."
  (equal (car cons1) (car cons2)))


;;; "keg version"

(cort-deftest-with-shell-command keg/version/without-argument/text
  '(("./bin/keg version"
     "Keg [0-9.]+ running on Emacs [0-9.]+")))

(cort-deftest keg/version/without-argument/exit-code
  '((:car-equal
     (let* ((command (expand-file-name "bin/keg"))
            (default-directory (expand-file-name "./test-data")))
       (shell-command-return-tuple
        (mapconcat #'shell-quote-argument
                   (list command
                         "version")
                   " ")))
     '(0 "" ""))))


;;; "keg help"

(cort-deftest-with-shell-command keg/help/without-argument/text
  '(("./bin/keg help"
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
     "^ run \\[SCRIPT\\]$"
     "^ version$")))

(cort-deftest keg/help/without-argument/exit-code
  '((:car-equal
     (let* ((command (expand-file-name "bin/keg"))
            (default-directory (expand-file-name "./test-data")))
       (shell-command-return-tuple
        (mapconcat #'shell-quote-argument
                   (list command
                         "help")
                   " ")))
     '(0 "" ""))))


;;; "keg lint"

(cort-deftest keg/lint/without-argument/exit-code
  '((:car-equal
     (let* ((command (expand-file-name "bin/keg"))
            (default-directory (expand-file-name "./test-data")))
       (shell-command-return-tuple
        (mapconcat #'shell-quote-argument
                   (list command
                         "lint")
                   " ")))
     '(0 "" ""))))

;; Inner functions

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
       (macro-&rest-args x 1 2))
     (progn
       (let ((x 1)) x)
       (let ((x 1)) x 1 2)))))

(cort-deftest-generate keg/with-keg-ansi :string=
  '(((with-keg-ansi
      (red "foo"))
     (keg-ansi 'red "foo"))
    ((with-keg-ansi
      (blue "foo"))
     (keg-ansi 'blue "foo"))
    ((with-keg-ansi
      (red "foo")
      (blue "bar"))
     (concat (keg-ansi 'red "foo")
             (keg-ansi 'blue "bar")))
    ((with-keg-ansi
      (red "foo")
      (blue "bar")
      (green "baz"))
     (concat (keg-ansi 'red "foo")
             (keg-ansi 'blue "bar")
             (keg-ansi 'green "baz")))))

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
