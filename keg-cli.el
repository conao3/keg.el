;;; keg-cli.el --- Parse CLI args  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'subr-x)

(defgroup keg-cli nil
  "Parse CLI arguments."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))

(defvar keg-cli-name nil)
(defvar keg-cli-options nil)
(defvar keg-cli-commands nil)
(defvar keg-cli-description nil)
(defvar keg-cli-default-config nil)
(defvar keg-cli-default-command nil)
(defvar keg-cli-no-command nil)

(defvar keg-cli-args nil)
(defvar keg-cli-parsing-done nil)

(defconst keg-cli-option-re
  "\\(-[A-Za-z0-9-]\\|--?[A-Za-z0-9][A-Za-z0-9-]+\\)"
  "Regex matching an option flag.")

(defconst keg-cli-command-re
  "\\([A-Za-z0-9][A-Za-z0-9-]*\\)"
  "Regex matching an command.")



(defmacro keg-cli--aliaslet (bindings &rest body)
  "Make temporary macro definitions.
This is like `cl-flet', but for macros instead of functions for BODY.

BINDINGS is list of definition.
The definition is list like (origfn aliasfn arglist).

\(fn ((NAME ALIAS ARGLIST) ...) FORM...)"
  (declare (indent 1))
  `(cl-macrolet ,(mapcar
                  (lambda (elm)
                    `(,(nth 0 elm)
                      ,(nth 2 elm)
                      `(apply
                        #',',(nth 1 elm)
                        ',(list ,@(cl-set-difference (nth 2 elm) '(&optional &rest))))))
                  bindings)
     ,@body))

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
  "Make proper command/option args from ARGS.

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

(defun keg-cli-option (flags desc func default-values)
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

(defun keg-cli-command (command desc func default-values)
  "Interpret command op.
With COMMAND, DESC, FUNC, DEFAULT-VALUES."
  (let* (required
         optional
         zero-or-more
         one-or-more
         (to-string command))
    (let ((matches (string-match (concat "\\`" keg-cli-command-re " " "<\\(.+\\)>" "\\'") command)))
      (when matches
        (setq command (nth 1 matches))
        (when (nth 2 matches)
          (setq required t)
          (if (equal (nth 2 matches) "*")
              (setq one-or-more t)))))
    (let ((matches (string-match (concat "\\`" keg-cli-command-re " " "\\[\\(.+\\)\\]" "\\'") command)))
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
      (setq keg-cli-default-config
            (keg--flatten (mapcar (lambda (elm) (split-string elm " ")) lines))))))

(defun keg-cli--usage-command-or-option (str desc)
  "`keg-cli--usage-command-or-option' with STR and DESC."
  (unless (listp desc)
    (setq desc (list desc)))
  (let* ((lst (append
               (mapcar (lambda (elm) (length (keg-cli-option-to-string elm))) keg-cli-options)
               (mapcar (lambda (elm) (length (keg-cli-command-to-string elm))) keg-cli-commands)))
         (padding (+ 10 (apply #'max lst))))
    (concat
     " "
     str
     (make-string (- padding (length str)) ?\s)
     (car desc)
     (string-join
      (mapcar
       (lambda (elm) (concat "\n" (make-string (1+ padding) ?\s) elm))
       (cdr desc))))))

(defun keg-cli--usage-command (command)
  "`keg-cli--usage-command' with COMMAND."
  (let ((to-string (keg-cli-command-to-string command))
        (description (keg-cli-command-description command)))
    (keg-cli--usage-command-or-option to-string description)))

(defun keg-cli--usage-option (option)
  "`keg-cli--usage-option' with OPTION."
  (let ((to-string (keg-cli-option-to-string option))
        (description (keg-cli-option-description option)))
    (keg-cli--usage-command-or-option to-string description)))

(defun keg-cli-usage ()
  "Return usage information as a string."
  (let ((name (or keg-cli-name (file-name-nondirectory load-file-name)))
        (commands-string
         (string-join (mapcar (lambda (elm) (keg-cli--usage-command elm)) (keg-cli--usage-commands)) "\n"))
        (options-string
         (string-join (mapcar (lambda (elm) (keg-cli--usage-option elm)) (keg-cli--usage-options)) "\n")))
    (concat
     (format "USAGE: %s [COMMAND] [OPTIONS]" name)
     (when keg-cli-description
       (format "\n\n%s" keg-cli-description))
     (when keg-cli-commands
       (format "\n\nCOMMANDS:\n\n%s" commands-string))
     (when keg-cli-options
       (format "\n\nOPTIONS:\n\n%s" options-string)))))

(defun keg-cli-default (cmd args)
  "`keg-cli-default' with CMD, ARGS."
  (if (stringp cmd)
      (setq
       keg-cli-default-command
       (make-keg-cli-default-command
        :command cmd
        :arguments args))
    (setq
     keg-cli-no-command
     (make-keg-cli-no-command
      :function cmd
      :arguments args))))

(defun keg-cli-parse (args)
  "`keg-cli-parse' with ARGS."
  (unless (bound-and-true-p commander-ignore)
    (let* ((rest-config (keg-cli--handle-options keg-cli-default-config))
           (rest (or (keg-cli--handle-options args) rest-config)))
      (unless rest
        (if keg-cli-default-command
            (let ((command (keg-cli-default-command-command keg-cli-default-command))
                  (args (keg-cli-default-command-arguments keg-cli-default-command)))
              (setq rest (cons command args)))))
      (keg-cli--handle-command rest)))
  (setq keg-cli-parsing-done t))

(defmacro define-keg-cli (name &rest body)
  "Define command parser.
NAME is command name used help command.
BODY is `keg-cli' command definition DSL."
  (declare (indent 1))
  `(progn
     (setq keg-cli-name ,(symbol-name name))
     (keg-cli--aliaslet
         ((option      keg-cli-option (flags desc func &rest default-values))
          (command     keg-cli-command (command desc func &rest default-values))
          (description keg-cli-description (desc))
          (config      keg-cli-config (file))
          (default     keg-cli-default (cmd default-values))
          (parse       keg-cli-parse (args)))
       ,@body)
     (unless keg-cli-parsing-done
       (keg-cli-parse (or keg-cli-args (cdr command-line-args-left))))))

(provide 'keg-cli)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg-cli.el ends here
