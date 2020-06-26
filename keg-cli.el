;;; keg-cli.el --- Parse CLI arguments  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

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

;; Parse CLI arguments.

;; Core concept and some codes from commander.el (GPLv3) by Johan Andersson.
;;   https://github.com/rejeep/commander.el


;;; Code:

(defgroup keg-cli nil
  "Parse CLI arguments."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))

(defvar keg-cli-name nil)
(defvar keg-cli-args nil)
(defvar keg-cli-parsing-done nil)

(defun keg-cli--make-args (args))

(defun keg-cli-option (args))
(defun keg-cli-command (args))
(defun keg-cli-description (desc))
(defun keg-cli-config (file))
(defun keg-cli-usage ())
(defun keg-cli-default (cmd args))
(defun keg-cli-parse (args))

(defmacro def-keg-cli (name &rest body)
  "Define command parser.
NAME is command name used help command.
BODY is `keg-cli' command definition DSL."
  (declare (indent 1))
  `(progn
     (setq keg-cli-name ',name)
     ,(mapcar
       (lambda (elm)
         (pcase elm
           (`(option . ,args)
            (apply #'keg-cli-option (keg-cli--make-args args)))
           (`(command . ,args)
            (apply #'keg-cli-command (keg-cli--make-args args)))
           (`(parse ,args)
            (keg-cli-parse args)
            (setq keg-cli-parsing-done t))
           (`(description ,desc)
            (keg-cli-description desc))
           (`(config ,file)
            (keg-cli-config file))
           (`(default ,cmd . ,args)
            (keg-cli-default cmd args))
           (_
            (error "Unknown directive: %s" elm))))
       body)
     (unless keg-cli-parsing-done
       (keg-cli-parse (or keg-cli-args (cdr command-line-args-left))))))

(provide 'keg-cli)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg-cli.el ends here
