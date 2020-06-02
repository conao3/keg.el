;;; keg-ansi.el --- Utility for ANSI terminal escape codes  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2013 Johan Andersson
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

;; Utility for ANSI terminal escape codes.

;; Core concept and some codes comes from ansi.el (GPLv3) by Johan Andersson.
;;   https://github.com/rejeep/ansi.el


;;; Code:

(defgroup keg-ansi nil
  "Utility for ANSI terminal escape codes."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))

(defconst keg-ansi-colors
  '((black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defconst keg-ansi-on-colors
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst keg-ansi-styles
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

(defconst keg-ansi-csis
  '((up       . "A")
    (down     . "B")
    (forward  . "C")
    (backward . "D"))
  "List of cursor navigation.")

(defconst keg-ansi-reset 0 "Ansi code for reset.")



(defun keg-ansi--code (effect)
  "Return code for EFFECT."
  (or
   (cdr (assoc effect keg-ansi-colors))
   (cdr (assoc effect keg-ansi-on-colors))
   (cdr (assoc effect keg-ansi-styles))))

(defun keg-ansi--char (effect)
  "Return char for EFFECT."
  (cdr (assoc effect keg-ansi-csis)))

(defun keg-ansi-apply (effect-or-code format-string &rest objects)
  "Apply EFFECT-OR-CODE to text.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (let ((code (if (numberp effect-or-code)
                  effect-or-code
                (keg-ansi--code effect-or-code)))
        (text (apply 'format format-string objects)))
    (format "\e[%dm%s\e[%sm" code text keg-ansi-reset)))

(defun keg-ansi-csi-apply (effect-or-char &optional reps)
  "Apply EFFECT-OR-CHAR REPS (1 default) number of times."
  (let ((char (if (symbolp effect-or-char)
                  (keg-ansi--char effect-or-char)
                effect-or-char)))
    (format "\u001b[%d%s" (or reps 1) char)))

(provide 'keg-ansi)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg-ansi.el ends here
