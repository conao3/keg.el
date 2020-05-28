;;; keg-mode.el --- Major mode for editing Keg files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/conao3/keg.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.1

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

;; Major mode for editing Keg files.

;;; Code:

(require 'lisp-mode)

(defvar calculate-lisp-indent-last-sexp) ; lisp-mode: L888
(defun keg-indent-function (indent-point state)
  "Indent calculation function for seml.
at INDENT-POINT on STATE.  see original function `lisp-indent-function'."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((memq (intern-soft function) '(package))
               (lisp-indent-specform 1 state
                                     indent-point normal-indent))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state)))))))

(defvar keg-mode-font-lock-keywords
  `((,(regexp-opt
       '("sources" "package" "devdependencies")
       'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt
       '("gnu" "melpa-stable" "melpa" "org")
       'symbols)
     . font-lock-variable-name-face)
    ;; Constant values.
    (,(concat "\\_<:" lisp-mode-symbol-regexp "\\_>")
     (0 font-lock-builtin-face))))

;;;###autoload
(define-derived-mode keg-mode prog-mode "Keg"
  "Major mode for editing Keg files."
  (lisp-mode-variables t nil t)
  (setq font-lock-defaults '(keg-mode-font-lock-keywords))
  (setq lisp-indent-function 'keg-indent-function))

;;;###autoload
(add-to-list 'auto-mode-alist '("/Keg\\'" . keg-mode))

(provide 'keg-mode)
;;; keg-mode.el ends here
