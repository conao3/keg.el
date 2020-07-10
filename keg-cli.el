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
(defvar keg-cli-options nil)
(defvar keg-cli-commands nil)
(defvar keg-cli-description nil)
(defvar commander-default-config nil)

(defvar keg-cli-args nil)
(defvar keg-cli-parsing-done nil)

(defconst keg-cli-option-re
  "\\(-[A-Za-z0-9-]\\|--?[A-Za-z0-9][A-Za-z0-9-]+\\)"
  "Regex matching an option flag.")

(defconst keg-cli-command-re
  "\\([A-Za-z0-9][A-Za-z0-9-]*\\)"
  "Regex matching an command.")



(defun keg--flatten (lst)
  "Return flatten list of LST."
  (let (fn)
    (setq fn (lambda (lst) (if (atom lst) `(,lst) (mapcan fn lst))))
    (funcall fn lst)))

(defun keg-cli--string-trim (str &optional trim-left trim-right)
  "Trim STR of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (let ((res str))
    (setq res (replace-regexp-in-string (or trim-left "\\`[ \t\n\r]+") "" res))
    (setq res (replace-regexp-in-string (or trim-right "[ \t\n\r]+\\'") "" res))
    res))

(defun keg-cli--make-args (args)
  "Make proper command/option arguments from ARGS.

ARGS is the args that are passed to the `command' and `option'
directives.  The return value is a list complete list that can be
sent to `keg-cli-command' and `keg-cli-options'.

If ARGS does not contain documentation, it is fetched from the
function doc string."
  (when (functionp (nth 1 args))
    (let ((desc* (let ((desc (documentation (nth 1 args))))
                   (if desc
                       (split-string desc "\\(\r\n\\|[\n\r]\\)")
                     ""))))
      (push desc* (cdr args))))
  args)

(defun keg-cli-option (flags desc func &rest default-values)
  "Interpret option op.
With FLAGS, DESC, FUNC, DEFAULT-VALUES."
  (let (required optional zero-or-more one-or-more)
    (mapcar
     (lambda (flag)
       (let ((to-string flags))
         (let ((matches (string-match (concat "\\`" keg-cli-option-re " " "<\\(.+\\)>" "\\'") flag)))
           (when matches
             (setq flag (match-string 1 flag))
             (when (match-string 2 flag)
               (setq required t)
               (if (equal (match-string 2 flag) "*")
                   (setq one-or-more t)))))
         (let ((matches (string-match (concat "\\`" keg-cli-option-re " " "\\[\\(.+\\)\\]" "\\'") flag)))
           (when matches
             (setq flag (match-string 1 flag))
             (when (match-string 2 flag)
               (setq required t)
               (if (equal (match-string 2 flag) "*")
                   (setq one-or-more t)))))
         (push
          `((flag . ,flag)
            (flags . ,flags)
            (desc . ,desc)
            (func . ,func)
            (default-values . ,default-values)
            (required . ,required)
            (optional . ,optional)
            (zero-or-more . ,zero-or-more)
            (one-or-more . ,one-or-more)
            (to-string . ,to-string))
          keg-cli-options)))
     (mapcar 'keg-cli--string-trim (split-string flags ",")))))

(defun keg-cli-command (command desc func &rest args)
  "Interpret command op.
With COMMAND, DESC, FUNC, ARGS."
  (let* (required
         optional
         zero-or-more
         one-or-more
         (to-string command)
         (default-values (-take-while 'stringp args)))
    (let ((matches (s-match (concat "\\`" keg-cli-command-re " " "<\\(.+\\)>" "\\'") command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq required t)
          (if (equal (nth 2 matches) "*")
              (setq one-or-more t)))))
    (let ((matches (s-match (concat "\\`" keg-cli-command-re " " "\\[\\(.+\\)\\]" "\\'") command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq optional t)
          (if (equal (nth 2 matches) "*")
              (setq zero-or-more t)))))
    (push
     `((command . ,command)
       (description . ,desc)
       (func . ,func)
       (default-values . ,default-values)
       (required . ,required)
       (optional . ,optional)
       (zero-or-more . ,zero-or-more)
       (one-or-more . ,one-or-more)
       (to-string . ,to-string))
     keg-cli-commands)))

(defun keg-cli-description (desc)
  "Interpret command desc with DESC."
  (setq keg-cli-description desc))

(defun keg-cli-config (file)
  "Interpreg config op with FILE."
  (when (file-readable-p file)
    (let* ((contents (with-temp-buffer
                       (insert-file-contents file)
                       (split-string (buffer-string) "\\(\r\n\\|[\n\r]\\)")))
           (lines (cl-remove-if
                   (lambda (elm) (or (null elm) (string= "" elm)))
                   contents)))
      (setq commander-default-config
            (keg--flatten (mapcar (lambda (elm) (split-string elm " ")) lines))))))

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
     ,@(mapcar
        (lambda (elm)
          (pcase elm
            (`(option . ,args)
             `(keg-cli-option ',args))
            (`(command . ,args)
             `(keg-cli-command ',args))
            (`(parse ,args)
             `(progn
                (keg-cli-parse ',args)
                (setq keg-cli-parsing-done t)))
            (`(description ,desc)
             `(keg-cli-description ',desc))
            (`(config ',file)
             `(keg-cli-config ',file))
            (`(default ,cmd . ,args)
             `(keg-cli-default ',cmd ',args))
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
