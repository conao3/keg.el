;;; keg-command-lint.el --- Implementation for "keg lint"  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

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

;; Implementation for "keg lint"


;;; Code:
(require 'keg-core)

(defvar keg-linters '(package-lint byte-compile checkdoc)
  "List of checkers.")

(defvar keg-current-linters nil)

(defun keg-lint-run-1 (files)
  "Exec one of left linters for FILES."
  (let ((linter (pop keg-current-linters))
        (code 0)
        (running t))
    (when linter
      (let* ((fn (intern (format "keg-lint--%s-batch" linter)))
             (command `("emacs" "--batch"
                        "-l"
                        ,(expand-file-name "keg-lint.el" keg-directory)
                        ,(format "--funcall=%s" fn)
                        ,@files))
             (proc (apply #'keg-start-process command)))
        (set-process-sentinel
         proc
         (lambda (proc _event)
           (let ((pcode (process-exit-status proc)))
             (unless (= 0 pcode)
               (setq code pcode)))
           (setq running nil)
           (let ((pcode (keg-lint-run-1 files)))
             (unless (= 0 pcode)
               (setq code pcode)))))
        (while running
          (accept-process-output proc 0 100))))
    code))

(defun keg-lint (&optional package)
  "Exec linters for PACKAGE."
  (keg-install-package 'package-lint)
  (let ((section (keg-file-read-section 'packages))
        (linters keg-linters)
        (code 0))
    (dolist (elm (keg-file-read-section 'disables))
      (unless (memq elm linters)
        (warn "Linter %s is disabled, but definition is missing" elm))
      (setq linters (delq elm linters)))
    (dolist (info (if (not package)
                      section
                    (list (assoc package section))))
      (let* ((name (car info))
             (files (keg-elisp-files name)))
        (unless linters
          (warn "All linter are disabled"))
        (setq keg-current-linters linters)
        (let ((pcode (keg-lint-run-1 files)))
          (unless (= 0 pcode)
            (setq code pcode)))))
    code))

(provide 'keg-command-lint)
;;; keg-command-lint.el ends here
