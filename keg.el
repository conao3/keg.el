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
      (packages . ,(nreverse (delete-dups packages))))))


;;; Functions

(defun keg-princ (arg)
  "Do `princ' ARG with \n."
  (princ (format "%s\n" arg)))

(defun keg-load-path ()
  "Return `load-path' in the form of PATH."
  (mapconcat #'identity (mapcar #'shell-quote-argument load-path) ":"))


;;; Main

(defun keg-main-load-path ()
  "Return `load-path' in the form of PATH."
  (keg-princ (keg-load-path)))

(provide 'keg)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg.el ends here
