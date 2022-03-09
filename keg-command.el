;;; keg-command.el --- CLI subcommand definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Naoya Yamashita

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

;; CLI subcommand definition

;;; Code:

(provide 'keg-command)

(require 'keg)

(defvar keg-directory)

(defvar keg-version)

(defun keg--argument-count-check (num-min num-max subcommand args)
  "Check number of ARGS range NUM-MIN to NUM-MAX in SUBCOMMAND.
Meaning of -1 is no restriction."
  (let ((num (length args)))
    (unless (and (or (= -1 num-min)
                     (<= num-min num))
                 (or (= -1 num-max)
                     (<= num num-max)))
      (let ((usage (keg-argument-usage subcommand)))
        (error (concat
                "USAGE: keg"
                (format " %s" (if usage usage subcommand))
                "\n\n"
                (format "The `%s' subcommand expects %s to %s arguments
but currently %s arguments have been specified"
                        subcommand
                        (if (not (= -1 num-min)) num-min 0)
                        (if (not (= -1 num-max)) num-max 'inf)
                        num)))))))

(defun keg--argument-package-check (package &optional allow-nil)
  "Check PACKAGE is one of defined packages.
Return package symbol if package defined.
PACKAGE as string is also acceptable.
If ALLOW-NIL is non-nil, it don't warn if package is nil."
  (let ((packages (keg-packages))
        (pkg (if (not (stringp package)) package (intern package))))
    (if (not (memq pkg (keg-packages)))
        (unless allow-nil
          (warn "Package %s is not defined.  Package should one of %s" pkg packages))
      pkg)))

(defun keg-command-help (&rest args)
  "Show this help.
ARGS is CLI arguments.

USAGE: keg help"
  (keg--argument-count-check 0 0 'help args)
  (keg--princ
   "USAGE: keg [SUBCOMMAND] [OPTIONS...]

Modern Elisp package development system.

SUBCOMMANDS:")
  (dolist (elm (keg-subcommands))
    (let ((usage (keg-usage elm))
          (argument-usage (keg-argument-usage elm)))
      (keg--princ (format " %s" (if argument-usage argument-usage elm)))
      (keg--princ (keg--indent 5 usage)))))

(defun keg-command-version (&rest args)
  "Show `keg' version.
ARGS is CLI arguments

USAGE: keg version"
  (keg--argument-count-check 0 0 'version args)
  (keg--princ
   (format "Keg %s running on Emacs %s"
           keg-version
           emacs-version)))

(defun keg-command-init (&rest args)
  "Create Keg template file.
ARGS is CLI argument.

USAGE: keg init"
  (keg--argument-count-check 0 0 'install args)
  (when (file-exists-p "Keg")
    (error "Keg file already exists.  Do nothing"))
  (with-temp-file "Keg"
    (insert "\
(source gnu)
(source melpa)

(depends-on \"keg\")
(depends-on \"leaf\")
"))
  (keg--princ "Successful creating Keg file"))

(defun keg-command-install (&rest args)
  "Install PACKAGES dependencies in .keg sandbox folder.
ARGS is CLI argument.

USAGE: keg install [PACKAGES...]"
  (keg--argument-count-check -1 -1 'install args)
  (keg--princ "Install dependencies")
  (let ((reqinfo (keg-build--get-dependency-from-keg-file))
        (packages (cond
                   (args (mapcar #'keg--argument-package-check args))
                   ((getenv "KEGINSTALLPACKAGES")
                    (mapcar #'keg--argument-package-check (split-string (getenv "KEGINSTALLPACKAGES"))))
                   (t (keg-packages)))))
    (dolist (info (keg-file-read-section 'packages))
      (let* ((name (car info))
             (_alist (cdr info))
             (reqs (keg--alist-get name reqinfo)))
        (when (memq name packages)
          (keg--princ (format " Package: %s" name))
          (keg--princ (format "     Dependency: %s"
                              (mapcar
                               (lambda (elm)
                                 (let ((pkg (car elm))
                                       (ver (cadr elm)))
                                   `(,pkg ,(package-version-join ver))))
                               reqs))))))
    (keg--princ (format " DevDependency: %s"
                        (mapcar
                         (lambda (elm)
                           (let ((pkg (car elm))
                                 (ver (cadr elm)))
                             `(,pkg ,(package-version-join ver))))
                         (keg--alist-get 'keg--devs reqinfo))))
    (keg-around-script install
      (keg-build--resolve-dependency packages))))

(defun keg-command-exec (&rest args)
  "Exec COMMAND with appropriate environment variables.
ARGS is list of string.

USAGE: keg exec COMMAND [ARGS...]"
  (keg--argument-count-check 1 -1 'exec args)
  (keg-around-script exec
    (let ((proc (apply #'keg-start-process args)))
      (set-process-sentinel
       proc
       (lambda (proc _event)
         (kill-emacs (process-exit-status proc))))
      (while t                            ; wait acync process
        (accept-process-output proc 0 100)))))

(defun keg-command-emacs (&rest args)
  "Exec Emacs with appropriate environment variables.
Exec Emacs with ARGS.

USAGE: keg Emacs [ARGS...]"
  (keg--argument-count-check -1 -1 'emacs args)
  (keg-around-script emacs
    (apply #'keg-command-exec "emacs" args)))

(defun keg-command-eval (&rest args)
  "Eval SEXP via batch Emacs with appropriate environment variables.
ARGS are (separated) SEXP.

USAGE: keg eval [SEXP]"
  (keg--argument-count-check -1 -1 'eval args) ; sexp is separated
  (when args
    (keg-around-script eval
      (keg-command-exec "emacs" "--batch"
                        (format "--eval=\"%s\"" (keg--string-join args " "))))))

(defun keg-command-lint (&rest args)
  "Exec linters for PACKAGE.
ARGS first value is specified package.

USAGE: keg lint [PACKAGE]"
  (keg--argument-count-check -1 1 'lint args)
  (keg-around-script lint
    (let ((pkg (keg--argument-package-check (car args) 'allow)))
      (kill-emacs (keg-lint pkg)))))

(defun keg-command-build (&rest args)
  "Byte compile for PACKAGE.
ARGS first value is specified package.

USAGE: keg build [PACKAGE]"
  (keg--argument-count-check -1 1 'build args)
  (keg-around-script build
    (dolist (file (keg-elisp-files (car args)))
      (if (fboundp 'byte-recompile-file)
          (byte-recompile-file file 'force 0)
        (byte-compile-file file)))))

(defun keg-command-clean-elc (&rest args)
  "Clean `.elc' files.
ARGS first value is specified package.

USAGE: keg clean-elc [PACKAGE]"
  (keg--argument-count-check -1 1 'clean-elc args)
  (keg-around-script clean-elc
    (let ((pkg (keg--argument-package-check (car args) 'allow)))
      (dolist (file (keg-elisp-files pkg))
        (let ((elc (concat file "c")))
          (keg--princ (format "Removing %s..." elc))
          (ignore-errors (delete-file elc)))))))

(defun keg-command-clean (&rest args)
  "Clean `.elc' files and `.keg' sandbox.
ARGS is CLI argument.

USAGE: keg clean"
  (keg--argument-count-check 0 0 'clean args)
  (keg-around-script clean
   (keg--princ "Removing .keg...")
   (delete-directory ".keg" 'force)
   (keg-command-clean-elc)))

(defun keg-command-info (&rest args)
  "Show PACKAGE information.
ARGS first value is specified package.

USAGE: keg info [PACKAGE]"
  (keg--argument-count-check -1 1 'info args)
  (let ((reqinfo (keg-build--get-dependency-from-keg-file))
        (section (keg-file-read-section 'packages))
        (pkg (keg--argument-package-check (car args) 'allow)))
    (when (and pkg (not (keg--alist-get pkg section)))
      (error "%s is not defined.  PACKAGE should one of %s" pkg (keg-packages)))
    (dolist (info (if (not pkg)
                      section
                    (list (assoc pkg section))))
      (let* ((name (car info))
             (alist (cdr info))
             (reqs (keg--alist-get name reqinfo)))
        (keg--princ (format " Package: %s" name))
        (keg--princ (format "     Recipe: %s" (keg--alist-get 'recipe alist)))
        (keg--princ (format "     Dependency: %s"
                            (mapcar
                             (lambda (elm)
                               (let ((pkg (car elm))
                                     (ver (cadr elm)))
                                 `(,pkg ,(package-version-join ver))))
                             reqs)))))
    (keg--princ (format " DevDependency: %s"
                        (mapcar
                         (lambda (elm)
                           (let ((pkg (car elm))
                                 (ver (cadr elm)))
                             `(,pkg ,(package-version-join ver))))
                         (keg--alist-get 'keg--devs reqinfo))))))

(defun keg-command-load-path (&rest args)
  "Show Emacs appropriate `load-path' same format as PATH.
ARGS is CLI argument.

USAGE: keg `load-path'"
  (keg--argument-count-check 0 0 'load-path args)
  (keg--princ (keg-load-path)))

(defun keg-command-files (&rest args)
  "Show files associated with PACKAGE.
ARGS is specified package.

USAGE: keg files [PACKAGE]"
  (keg--argument-count-check -1 1 'files args)
  (let ((pkg (keg--argument-package-check (car args) 'allow)))
    (dolist (elm (keg-files pkg))
      (keg--princ elm))))

(defun keg-command-elisp-files (&rest args)
  "Show Elisp files associated with PACKAGE.
ARGS is specified package.

USAGE: keg files [PACKAGE]"
  (keg--argument-count-check -1 1 'files args)
  (let ((pkg (keg--argument-package-check (car args) 'allow)))
    (dolist (elm (keg-elisp-files pkg))
      (keg--princ elm))))

(defun keg-command-debug (&rest args)
  "Show debug information.
ARGS is CLI argument.

USAGE: keg debug"
  (keg--argument-count-check 0 0 'debug args)
  (keg--princ "Keg debug information")
  (let ((reqinfo (keg-build--get-dependency-from-keg-file)))
    (dolist (info (keg-file-read-section 'packages))
      (let* ((name (car info))
             (alist (cdr info))
             (reqs (keg--alist-get name reqinfo)))
        (keg--princ (format " Package: %s" name))
        (keg--princ (format "     Recipe: %s" (keg--alist-get 'recipe alist)))
        (keg--princ (format "     Dependency: %s"
                            (mapcar
                             (lambda (elm)
                               (let ((pkg (car elm))
                                     (ver (cadr elm)))
                                 `(,pkg ,(package-version-join ver))))
                             reqs)))))
    (keg--princ (format " DevDependency: %s"
                        (mapcar
                         (lambda (elm)
                           (let ((pkg (car elm))
                                 (ver (cadr elm)))
                             `(,pkg ,(package-version-join ver))))
                         (keg--alist-get 'keg--devs reqinfo)))))
  (keg--princ " Keg file: %s" (keg-file-path))
  (keg--princ " Keg file parsed")
  (keg--princ (keg--indent 5 (pp-to-string (keg-file-read)))))

(defun keg-command-run (&rest args)
  "Run script named SRCIPT defined in Keg file.
ARGS is name of script.

USAGE: keg run [SCRIPT]"
  (keg--argument-count-check 1 1 'run args)
  (kill-emacs (keg-run-script (car args))))

(defvar keg-global-commands '(init help version debug)
  "List of commands that don't require a Keg file.")

(defvar keg-no-install-commands `(,@keg-global-commands
                                  install clean clean-elc info)
  "List of commands that don't require dependency installation.")


(defun keg-command ()
  "Init `keg' and exec subcommand."
  (unless noninteractive
    (error "`keg-command' is to be used only with --batch"))
  (let* ((opraw (car command-line-args-left))
         (op (when opraw (intern opraw)))
         (args (cdr command-line-args-left))
         (no-install (keg--no-install-p op keg-no-install-commands)))
    (when (not (memq op keg-global-commands))
        (keg-initialize))
    (cond
     ((and
       (memq op keg-global-commands)
       (not (file-exists-p "Keg")))
      (keg--princ "Missing Keg file in current directory")
      (keg--princ "Exec `keg init' to create Keg file")
      (keg--princ ""))
     ((and
       (not (memq op keg-global-commands))
       (not (file-exists-p "Keg")))
      (keg--princ "Missing Keg file in current directory")
      (keg--princ "Keg file is required to exec `%s' command" op)
      (keg--princ "Exec `keg init' to create Keg file")
      (kill-emacs 1))
     ((and
       (not no-install)
       (not (file-directory-p user-emacs-directory)))
      (keg--princ "As missing .keg sandbox, install dependencies")
      (make-directory user-emacs-directory 'parent)
      (keg-command-install))
     ((not no-install)
      (make-directory user-emacs-directory 'parent)
      (keg-command-install)))

    (cond
     ((null op))             ;Do nothing because `keg-command-install' is already run.
     ((memq op (keg-subcommands))
      (apply (intern (format "keg-command-%s" (symbol-name op))) args))
     (t
      (keg-command-help)
      (keg--princ)
      (error (format "Subcommand `%s' is not defined" op))))))

;;; keg-command.el ends here
