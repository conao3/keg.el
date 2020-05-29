;;; keg.el --- Modern Elisp package development system  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
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

;; Modern Elisp package development system


;;; Code:

(defgroup keg nil
  "Modern Elisp package development system."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))


;;; Keg file

(defun keg-file-dir ()
  "Get directory path which Keg located search from `deafult-directory'.
If no found the directory, returns nil."
  (locate-dominating-file default-directory "Keg"))

(defun keg-file-path ()
  "Get file path to Keg file search from `deafult-directory'.
If no found the directory, returns nil."
  (let ((kegdir (keg-file-dir)))
    (when kegdir (expand-file-name "Keg" kegdir))))

(defun keg-file-read ()
  "Return sexp from Keg file search from `deafult-directory'.
If no found the Keg file, returns nil."
  (let ((path (keg-file-path))
        sources devs packages)
    (when path
      (dolist (elm (read (with-temp-buffer
                           (insert-file-contents path)
                           (format "(%s)" (buffer-string)))))
        (let ((op (car elm))
              (args (cdr elm)))
          (cond
           ((eq 'sources op)
            (dolist (elm args) (push elm sources)))
           ((eq 'dev-dependencies op)
            (dolist (elm args) (push elm devs)))
           ((eq 'package op)
            (push args packages)))))
      `((sources . ,(nreverse (delete-dups sources)))
        (devs . ,(nreverse (delete-dups devs)))
        (packages . ,(nreverse (delete-dups packages)))))))


;;; Functions

(defun keg--princ (&optional str &rest args)
  "Do `princ' STR with format ARGS and put \n."
  (when str (princ (apply #'format str args)))
  (princ "\n"))

(defun keg--indent (width str)
  "Add indent of WIDTH for STR each lines."
  (replace-regexp-in-string "^" (make-string width ?\s) str))

(defun keg--alist-get (key alist &optional default _remove testfn)
  "Find the first element of ALIST whose `car' equals KEY and return its `cdr'.
If KEY is not found in ALIST, return DEFAULT.
Equality with KEY is tested by TESTFN, defaulting to `eq'.

This function is `alist-get' polifill for Emacs < 25.1."
  (let ((x (if (not testfn)
               (assq key alist)
             (assoc key alist testfn))))
    (if x (cdr x) default)))

(defun keg-load-path ()
  "Return `load-path' in the form of PATH."
  (mapconcat #'identity (mapcar #'shell-quote-argument load-path) ":"))

(defun keg-subcommands ()
  "Return keg subcommands."
  (let (res)
    (mapatoms
     (lambda (elm)
       (when (and (fboundp elm)
                  (string-prefix-p "keg-main-" (symbol-name elm)))
         (push (intern (replace-regexp-in-string "^keg-main-" "" (symbol-name elm))) res))))
    (sort res (lambda (a b) (string< (symbol-name a) (symbol-name b))))))


;;; Main

(defun keg-main-help ()
  "Show this help."
  (keg--princ
   "USAGE: keg [SUBCOMMAND] [OPTIONS...]

Modern Elisp package development system

SUBCOMMANDS:")
  (keg--princ
   (mapconcat
    (lambda (elm)
      (format
       " %s\n%s"
       elm
       (keg--indent 5 (documentation (intern (format "keg-main-%s" elm))))))
    (keg-subcommands)
    "\n")))

(defun keg-main-version ()
  "Show `keg' version."
  (keg--princ
   (format "Keg %s"
           (eval-when-compile
             (require 'lisp-mnt)
             (lm-version (or load-file-name
                             byte-compile-current-file))))))

(defun keg-main-init ()
  "Create Keg template file."
  (when (file-exists-p "Keg")
    (error "Keg file already exists.  Do nothing"))
  (with-temp-file "Keg"
    (insert "\
(source gnu)
(source melpa)

(depends-on \"keg\")
(depends-on \"leaf\")
"))
  (keg--princ "Successful creating Keg file"))

(defun keg-main-info ()
  "Show this package information."
  (keg--princ "Keg file parsed")
  (keg--princ (pp-to-string (keg-file-read))))

(defun keg-main-debug ()
  "Show debug information."
  (keg--princ "Keg debug information")
  (dolist (info (keg--alist-get 'packages (keg-file-read)))
    (let ((name (car info))
          (alist (cdr info)))
      (keg--princ (format " Package: %s" name))
      (keg--princ (format " Recipe: %s" (keg--alist-get 'recipe alist)))
      (keg--princ)))
  (keg--princ " Keg file")
  (keg--princ (keg--indent 5 (keg-file-path)))
  (keg--princ)
  (keg--princ " Keg file parsed")
  (keg--princ (keg--indent 5 (pp-to-string (keg-file-read)))))

(defun keg-main-load-path ()
  "Return `load-path' in the form of PATH."
  (keg--princ (keg-load-path)))

(defun keg-main ()
  "Init `keg' and exec subcommand."
  (let ((op (car command-line-args-left))
        (args (cdr command-line-args-left)))
    (cond
     ((eq nil op)
      (keg-main-help)
      (kill-emacs 1))
     ((memq (intern op) (keg-subcommands))
      (apply (intern (format "keg-main-%s" op)) args))
     (t
      (keg--princ (format "Subcommand `%s' is missing" op))
      (keg--princ)
      (keg-main-help)
      (kill-emacs 1)))))

(provide 'keg)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg.el ends here
