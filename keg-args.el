;;; keg-args.el --- Parse CLI arguments  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/keg-args.el

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

(defgroup keg-args nil
  "Parse CLI arguments."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))

(defvar keg-args-name nil)

(defmacro def-keg-args (name &rest body)
  "Define command parser.
NAME is command name used help command.
BODY is `keg-args' command definition DSL."
  `(progn
     (setq keg-args-name ,name)
     ,(mapcar
       (lambda (elm) elm)
       body)))

(provide 'keg-args)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg-args.el ends here
