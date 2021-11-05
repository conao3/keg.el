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

(require 'macroexp)

(defgroup keg-ansi nil
  "Utility for ANSI terminal escape codes."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))

(defconst keg-ansi-codes
  '((reset     . 0)
    (bold      . 1)
    (faint     . 2)
    (italic    . 3)
    (underline . 4)
    (blink     . 5)
    (r-blink   . 6) (rapid-blink . 6)
    (invert    . 7)
    (conceal   . 8)
    (strike    . 9)

    (font-0    . 10) (default-font . 10)
    (font-1    . 11)
    (font-2    . 12)
    (font-3    . 13)
    (font-4    . 14)
    (font-5    . 15)
    (font-6    . 16)
    (font-7    . 17)
    (font-8    . 18)
    (font-9    . 19)
    (font-10   . 20)

    (bold-off      . 21) (d-underline . 21) (double-underline . 21)
    (faint-off     . 22)
    (italic-off    . 23)
    (underline-off . 24)
    (blink-off     . 25)
    (r-blink-off   . 26) (rapid-blink-off . 26)
    (invert-off    . 27)
    (conceal-off   . 28)
    (strike-off    . 29)

    (black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37)
    ;; ( . 38)                ; 256 color / 24bit color
    (default . 39)

    (black-bg   . 40)
    (red-bg     . 41)
    (green-bg   . 42)
    (yellow-bg  . 43)
    (blue-bg    . 44)
    (magenta-bg . 45)
    (cyan-bg    . 46)
    (white-bg   . 47)
    ;; ( . 48)                ; 256 color / 24bit color
    (default-bg . 49)

    ;; ( . 50)
    (frame . 51)
    (circle . 52)
    (overline . 53)
    (frame-off . 54) (circle-off . 54)
    (overline-off . 55)

    ;; ( . 56)
    ;; ( . 57)
    ;; ( . 58)
    ;; ( . 59)

    (ideogram-underline . 60) (right-side-line . 60)
    (ideogram-double-underline . 61) (double-line-right-side . 61)
    (ideogram-overline . 62) (left-side-line . 62)
    (ideogram-double-overline . 63) (double-line-left-side . 63)
    (ideogram-stress-marking . 64)
    (ideogram-off . 65)

    ;; ( . 66) - ( . 89)

    (b-black   . 90) (bright-black   . 90)
    (b-red     . 91) (bright-red     . 91)
    (b-green   . 92) (bright-green   . 92)
    (b-yellow  . 93) (bright-yellow  . 93)
    (b-blue    . 94) (bright-blue    . 94)
    (b-magenta . 95) (bright-magenta . 95)
    (b-cyan    . 96) (bright-cyan    . 96)
    (b-white   . 97) (bright-white   . 97)
    ;; ( . 98)
    (b-default . 99) (bright-default . 97)

    (b-black-bg   . 100) (bright-black-bg   . 100)
    (b-red-bg     . 101) (bright-red-bg     . 101)
    (b-green-bg   . 102) (bright-green-bg   . 102)
    (b-yellow-bg  . 103) (bright-yellow-bg  . 103)
    (b-blue-bg    . 104) (bright-blue-bg    . 104)
    (b-magenta-bg . 105) (bright-magenta-bg . 105)
    (b-cyan-bg    . 106) (bright-cyan-bg    . 106)
    (b-white-bg   . 107) (bright-white-bg   . 107)
    ;; ( . 108)
    (b-default-bg . 109) (bright-default-bg . 109))
  "List of SGR (Select graphic rendition) codes.
See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters")

(defconst keg-ansi-csis
  '((up         . "A")
    (down       . "B")
    (forward    . "C")
    (backward   . "D")
    (ahead-down . "E") (beginning-of-line-down . "E")
    (ahead-up   . "F") (beginning-of-line-up   . "F")
    (column     . "G") (move-at-column . "G")
    (point      . "H") (move-at-point . "H") ; require 2 arguments (x,y)

    (clear      . "J")
    ;; 0 (default): clear forward all
    ;; 1: clear behind all
    ;; 2: clear all
    (clear-line . "K")
    ;; 0 (default): clear forward
    ;; 1: clear behind
    ;; 2: clear line

    (scroll-next . "S")
    (scroll-back . "T"))
  "List of CSI (Control sequence introducer) codes.
See https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_sequences")



(defun keg-ansi--macroexp-progn (exps)
  "Return EXPS (a list of expressions) with `progn' prepended.
If EXPS is a list with a single expression, `progn' is not
prepended, but that expression is returned instead.

Copied from `macroexp-progn'."
  (if (cdr exps) `(progn ,@exps) (car exps)))

(defmacro keg-ansi--cl-macrolet (bindings &rest body)
  "Make temporary macro definitions.
This is like `cl-flet', but for macros instead of functions.

see `cl-macrolet' for BINDINGS, BODY info.

\(fn ((NAME ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1))
  (if (null bindings)
      (keg-ansi--macroexp-progn body)
    (macroexpand-all (keg-ansi--macroexp-progn body)
                     (append
                      (mapcar
                       (lambda (binding)
                         (let* ((name (car binding))
                                (args (car (cdr binding)))
                                (macrobody (cdr (cdr binding))))
                           (cons name
                                 (eval `(lambda ,args ,@macrobody) t))))
                       bindings)
                      macroexpand-all-environment))))

(defun keg-ansi--alist-get (key alist &optional default)
  "Find the first element of ALIST whose `car' equals KEY and return its `cdr'.
If KEY is not found in ALIST, return DEFAULT.
For backward compatibility, TESTFN is always `eq'.

This function is `alist-get' polifill for Emacs < 25.1."
  (declare (indent 1))
  (let ((x (assq key alist)))
    (if x (cdr x) default)))

(defun keg-ansi (effect-or-code format-string &rest objects)
  "Apply EFFECT-OR-CODE to text.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (declare (indent 1))
  (let* ((str (if (stringp format-string)
                  format-string
                (prin1-to-string format-string)))
         (code (if (numberp effect-or-code)
                   effect-or-code
                 (keg-ansi--alist-get effect-or-code keg-ansi-codes)))
         (text (apply 'format str objects)))
    (format "\e[%dm%s\e[0m" code text)))

(defun keg-ansi-csi (effect-or-char &rest args)
  "Apply EFFECT-OR-CHAR ARGS (1 default) number of times."
  (declare (indent 1))
  (let ((code (if (stringp effect-or-char)
                  effect-or-char
                (keg-ansi--alist-get effect-or-char keg-ansi-csis))))
    (concat "\e[" (when args (mapconcat #'prin1-to-string args ";")) code)))

(defun keg-ansi-256 (code format-string &rest objects)
  "Apply 256-color CODE to text.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (declare (indent 1))
  (let* ((str (if (stringp format-string)
                  format-string
                (prin1-to-string format-string)))
         (text (apply 'format str objects)))
    (format "\e[38;5;%sm%s\e[0m" code text)))

(defun keg-ansi-256-bg (code format-string &rest objects)
  "Apply 256-color CODE to text background.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (declare (indent 1))
  (let* ((str (if (stringp format-string)
                  format-string
                (prin1-to-string format-string)))
         (text (apply 'format str objects)))
    (format "\e[48;5;%sm%s\e[0m" code text)))

(defun keg-ansi-rgb (r g b format-string &rest objects)
  "Apply R G B color to text.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (declare (indent 3))
  (let* ((str (if (stringp format-string)
                  format-string
                (prin1-to-string format-string)))
         (code (mapconcat #'prin1-to-string (list r g b) ";"))
         (text (apply 'format str objects)))
    (format "\e[38;2;%sm%s\e[0m" code text)))

(defun keg-ansi-rgb-bg (r g b format-string &rest objects)
  "Apply R G B color to text background.
FORMAT-STRING and OBJECTS are processed same as `apply'."
  (declare (indent 3))
  (let* ((str (if (stringp format-string)
                  format-string
                (prin1-to-string format-string)))
         (code (mapconcat #'prin1-to-string (list r g b) ";"))
         (text (apply 'format str objects)))
    (format "\e[48;2;%sm%s\e[0m" code text)))

(defmacro with-keg-ansi (&rest body)
  "Exec BODY with `keg-ansi' DSL."
  (let* ((exps (macroexpand-all
                `(keg-ansi--cl-macrolet
                     (,@(mapcar
                         (lambda (name)
                           `(,name (format-string &rest args)
                                   `(keg-ansi ',',name ,format-string ,@args)))
                         (mapcar #'car keg-ansi-codes))
                      ,@(mapcar
                         (lambda (name)
                           `(,name (&rest args)
                                   `(keg-ansi-csi ',',name ,@args)))
                         (mapcar #'car keg-ansi-csis))
                      (256-color (code format-string &rest objects)
                           `(keg-ansi-256 ,code ,format-string ,@objects))
                      (256-bg (code format-string &rest objects)
                              `(keg-ansi-256-bg ,code ,format-string ,@objects))
                      (rgb-color (r g b format-string &rest objects)
                           `(keg-ansi-rgb ,r ,g ,b ,format-string ,@objects))
                      (rgb-bg (r g b format-string &rest objects)
                              `(keg-ansi-rgb-bg ,r ,g ,b ,format-string ,@objects)))
                   magic-spacer         ; must be wrap progn
                   ,@body)
                macroexpand-all-environment)))
    `(concat ,@(cddr exps))))           ; drop 'progn and spacer

(provide 'keg-ansi)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg-ansi.el ends here
