;;; keg-lint.el --- Linter included by keg           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Naoya Yamashita, ROCKTAKEY

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Author: ROCKTAKEY <rocktakey@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Independent linter included by keg.

;;; Code:

(declare-function package-lint-batch-and-exit-1 "ext:package-lint")

(defvar package-archives)
(defvar package-gnupghome-dir)
(defvar checkdoc-diagnostic-buffer)

(defun keg-lint--package-lint-batch ()
  "Run `package-lint' for files specified CLI arguments."
  (unless noninteractive
    (error "`keg-lint--package-lint-batch' is to be used only with --batch"))

  (setq user-emacs-directory (or (getenv "KEGLINTUSEREMACSDIRECTORY") user-emacs-directory))
  (setq package-user-dir (or (getenv "KEGLINTPACKAGEUSERDIR") package-user-dir))
  (setq package-archives (or (read (or (getenv "KEGLINTPACKAGEARCHIVES") "nil")) package-archives))
  (setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
  (package-initialize)
  (prog1 'package-lint
    (require 'package-lint)
    (defvar package-lint-main-file)
    (setq package-lint-main-file (when (< 1 (length command-line-args-left))
                                   (car command-line-args-left))))

  (let ((success (package-lint-batch-and-exit-1 command-line-args-left)))
    (kill-emacs (if success 0 1))))

(defun keg-lint--byte-compile-batch ()
  "Run `byte-compile-file' for files specified CLI arguments."
  (unless noninteractive
    (error "`keg-lint--byte-compile-batch' is to be used only with --batch"))

  (let ((code 0))
    (dolist (file command-line-args-left)
      (let (pcode)
        (ignore-errors (kill-buffer "*Compile-Log*"))
        (byte-compile-file file)
        (with-current-buffer (get-buffer-create "*Compile-Log*")
          (if (<= (- (point-max) (point)) 3)
              (setq pcode 0)
            (goto-char (point-min)) (forward-line 2)
            (replace-regexp-in-string "\\(^[\n]+\\)\\|\\([\n]+$\\)" ""
                                      (buffer-substring (point) (point-max)))
            (setq pcode 1)))
        (unless (= 0 pcode)
          (setq code pcode))))
    (kill-emacs code)))

(defun keg-lint--checkdoc-batch ()
  "Run `checkdoc' for files specified CLI arguments."
  (unless noninteractive
    (error "`keg-lint--checkdoc-batch' is to be used only with --batch"))
  (require 'checkdoc)
  (let ((checkdoc-diagnostic-buffer "*warn*"))
    (mapc
     ;; Copied from `checkdoc-file'
     (lambda (file)
       (with-current-buffer (find-file-noselect file)
         (checkdoc-current-buffer t)))
     command-line-args-left)
    (kill-emacs (if (get-buffer "*Warnings*") 1 0))))

(provide 'keg-lint)
;;; keg-lint.el ends here
