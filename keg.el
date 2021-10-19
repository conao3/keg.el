;;; keg.el --- Modern Elisp package development system  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1") (cl-lib "0.6"))
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

;; Modern Elisp package development system


;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'package)
(require 'keg-command)
(require 'keg-ansi)

(defgroup keg nil
  "Modern Elisp package development system."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))

(defconst keg-directory
  (eval-when-compile
    (expand-file-name
     (file-name-directory (or byte-compile-current-file
                              load-file-name
                              buffer-file-name))))
  "Path to keg root.")

(defvar keg-archives
  '((gnu . "https://elpa.gnu.org/packages/")
    (org . "https://orgmode.org/elpa/")
    (melpa . "https://melpa.org/packages/")
    (celpa . "https://celpa.conao3.com/packages/"))
  "Alist for symbol to ELPA url.")

(defvar keg-version
  (eval-when-compile
    (lm-version (expand-file-name "keg.el" keg-directory)))
  "Keg version.")



(defvar checkdoc-diagnostic-buffer)


;;; Keg file

(defun keg-file-dir ()
  "Get directory path which Keg located search from `deafult-directory'.
If no found the directory, returns nil."
  (locate-dominating-file default-directory "Keg"))

(defun keg-file-path ()
  "Get file path to Keg file search from `deafult-directory'.
If no found the directory, returns nil."
  (let ((kegdir (keg-file-dir)))
    (when kegdir (expand-file-name "Keg" kegdir))))

(defun keg-file-read ()
  "Return sexp from Keg file search from `deafult-directory'.
If no found the Keg file, returns nil."
  (let ((path (keg-file-path))
        sources devs packages lint-disables)
    (when path
      (dolist (elm (read (with-temp-buffer
                           (insert-file-contents path)
                           (format "(%s)" (buffer-string)))))
        (let ((op (car elm))
              (args (cdr elm)))
          (cond
           ((eq 'source op)
            (dolist (elm args) (push elm sources)))
           ((eq 'dev-dependency op)
            (dolist (elm args) (push elm devs)))
           ((eq 'package op)
            (dolist (elm args) (push elm packages)))
           ((eq 'disable-lint op)
            (dolist (elm args) (push elm lint-disables))))))
      `((sources . ,(nreverse (delete-dups sources)))
        (devs . ,(nreverse (delete-dups devs)))
        (packages . ,(nreverse (delete-dups packages)))
        (disables . ,(nreverse (delete-dups lint-disables)))))))

(defun keg-file-read-section (section)
  "Return SECTION value from Keg file."
  (keg--alist-get section (keg-file-read)))

(defun keg-home-dir ()
  "Get package ELPA dir."
  (let ((kegdir (keg-file-dir)))
    (when kegdir
      (expand-file-name (format ".keg/%s/" emacs-version) kegdir))))

(defun keg-elpa-dir ()
  "Get package ELPA dir."
  (let ((keghomedir (keg-home-dir)))
    (when keghomedir
      (expand-file-name "elpa/" keghomedir))))


;;; Resolve dependencies

(defconst keg-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.

See `package-build-default-files-spec' from MELPA package-build.")

(defun keg-build--expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error.

See `package-build-expand-file-specs' from MELPA package-build."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs lst)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (keg-build--expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (keg-build--expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.el\\.in\\'"
                                            ".el"
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

(defun keg-build--config-file-list (recipe)
  "Build full source file specification from RECIPE.
See `package-build--config-file-list' from MELPA package-build."
  (let ((file-list (plist-get (cdr recipe) :files)))
    (cond
     ((null file-list)
      keg-build-default-files-spec)
     ((eq :defaults (car file-list))
      (append keg-build-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun keg-build--expand-source-file-list (&optional recipe dir)
  "Resolve source file from RECIPE in DIR.
See `package-build--expand-source-file-list' from MELPA package-build."
  (cl-remove-if-not
   (lambda (elm) (string-match-p "\\.el$" elm))
   (mapcar #'car
           (keg-build--expand-file-specs
            (or dir default-directory)
            (keg-build--config-file-list recipe)))))

(defun keg-build--get-dependency-from-elisp-file (file)
  "Get package dependency from Package-Require header from FILE.
Duplicate requires are resolved by more restrictive."
  (if (not (file-readable-p file))
      (error "File %s is missing" file)
    (let ((reqs-str (lm-with-file file
                      (lm-header "package-requires"))))
      (when reqs-str
        (mapcar
         (lambda (elm)
           (let ((req (car elm))
                 (ver (cadr elm)))
             `(,req ,(version-to-list ver))))
         (read reqs-str))))))

(defun keg-build--get-dependency-from-keg-file ()
  "Get development package dependency from Keg located DIR for PKG.
Currently, ignore any args for development.

Return value is below form:
  <result>  := (<pkg-req>* <dev-req>)
  <pkg-req> := (<pkg> . (<req>*))
  <dev-req> := (keg--devs . (<req>*))
  <pkg>     := SYMBOL
  <req>     := (<req-pkg> <req-ver>)
  <req-pkg> := SYMBOL
  <req-ver> := LIST                   ; like `version-to-list'"
  (let* ((devs (keg-file-read-section 'devs))
         ret)
    (dolist (package (keg-file-read-section 'packages))
      (let* ((name (car package))
             (_args (cdr package))
             (main-file (format "%s.el" name)))
        (push `(,name . ,(keg-build--get-dependency-from-elisp-file main-file)) ret)))
    (push `(keg--devs . ,(mapcar (lambda (elm) `(,elm ,(version-to-list keg-version))) devs)) ret)
    (nreverse ret)))

(defun keg-build--package-archives (&optional syms)
  "Return appropriate `packages-archives' using Keg sources value.
If SYMS is omitted, assume ELPA symbol from reading Keg file.
See `keg-archives' for symbol url mapping."
  (mapcar
   (lambda (elm)
     (let ((url (keg--alist-get elm keg-archives)))
       (if (not url)
           (error "Source %s is unknown" elm)
         `(,(symbol-name elm) . ,url))))
   (or syms (keg-file-read-section 'sources))))

(defun keg-build--resolve-dependency ()
  "Fetch dependency in .keg folder.
See `package-install'."
  (let ((package-archives (keg-build--package-archives))
        (reqs-info (keg-build--get-dependency-from-keg-file))
        transaction)
    (dolist (info reqs-info)
      (let ((_name (car info))
            (reqs (cdr info)))
        (condition-case _err
            (package-download-transaction
             (setq transaction (package-compute-transaction nil reqs)))
          (error                     ; refresh and retry if error
           (package-refresh-contents)
           (package-download-transaction
            (setq transaction (package-compute-transaction nil reqs)))))))
    (unless transaction
      (keg--princ)
      (keg--princ "All dependencies already satisfied"))))


;;; Lint

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
                        "--eval=\"(require 'keg)\""
                        ,(format "--funcall=%s" fn)
                        ,@files))
             (proc (keg-start-process (keg--string-join command " "))))
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

(declare-function package-lint-batch-and-exit-1 "ext:package-lint")

(defun keg-lint--package-lint-batch ()
  "Run `package-lint' for files specified CLI arguments."
  (unless noninteractive
    (error "`keg-lint--package-lint-batch' is to be used only with --batch"))

  (prog1 'package
    (setq package-archives (keg-build--package-archives '(melpa gnu)))
    (keg-initialize))

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

  (keg-initialize)
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


;;; Functions

(defun keg--princ (&optional str &rest args)
  "Do `princ' STR with format ARGS and put \n."
  (when str (princ (if (stringp str)
                       (replace-regexp-in-string
                        "\\(\\`[\n\r]+\\)\\|\\([\n\r]+\\'\\)" ""
                        (apply #'format str args))
                     str)))
  (princ "\n"))

(defun keg--indent (width str)
  "Add indent of WIDTH for STR each lines."
  (declare (indent 1))
  (replace-regexp-in-string "^" (make-string width ?\s) str))

(defun keg--alist-get (key alist &optional default)
  "Find the first element of ALIST whose `car' equals KEY and return its `cdr'.
If KEY is not found in ALIST, return DEFAULT.
For backward compatibility, TESTFN is always `eq'.

This function is `alist-get' polifill for Emacs < 25.1."
  (declare (indent 1))
  (let ((x (assq key alist)))
    (if x (cdr x) default)))

(defun keg--string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR.
This function is `string-join' polifill for Emacs < 24.4."
  (mapconcat 'identity strings separator))

(defun keg-install-package (pkg)
  "Install PKG in .keg folder."
  (let ((package-archives (keg-build--package-archives '(gnu melpa))))
    (unless (package-installed-p pkg)
      (condition-case _err
          (package-install pkg)
        (error
         (package-refresh-contents)
         (package-install pkg))))))

(defun keg-subcommands ()
  "Return keg subcommands."
  (let (res)
    (mapatoms
     (lambda (elm)
       (when (and (fboundp elm) (string-prefix-p "keg-command-" (symbol-name elm)))
         (push (intern (replace-regexp-in-string "^keg-command-" "" (symbol-name elm))) res))))
    (sort res (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun keg-load-path ()
  "Return keg `load-path' same format as PATH."
  (pcase system-type
    (`windows-nt (keg--string-join load-path ";"))
    (_ (keg--string-join (mapcar #'shell-quote-argument load-path) ":"))))

(defun keg-process-environment ()
  "Return `process-environment' for keg."
  (cons (format "EMACSLOADPATH=%s" (keg-load-path)) process-environment))

(defun keg-start-process (&rest command)
  "Exec COMMAND and return process object."
  (keg--princ "Exec command: %s" (keg--string-join command " "))
  (let* ((process-environment (keg-process-environment))
         (proc (start-process-shell-command
                "keg"
                (generate-new-buffer "*keg*")
                (keg--string-join command " "))))
    (set-process-filter
     proc
     (lambda (_proc str)
       ;; (princ str #'external-debugging-output)
       (princ str)))
    (set-process-sentinel
     proc
     (lambda (proc _event)
       (kill-buffer (process-buffer proc))))
    proc))

(defun keg-packages ()
  "Return packages list."
  (mapcar #'car (keg-file-read-section 'packages)))

(defun keg-files (&optional package)
  "Return files list associated with PACKAGE."
  (let ((pkg (keg--argument-package-check package 'allow)))
    (keg-build--expand-source-file-list
     (keg--alist-get 'recipe
       (keg--alist-get pkg
         (keg-file-read-section 'packages))))))

(defun keg-elisp-files (&optional package)
  "Return elisp files list associated with PACKAGE."
  (let ((main-file (format "%s.el" package))
        (res (sort (cl-remove-if
                    (lambda (elm) (not (string-match "\\.el$" elm)))
                    (keg-files package))
                   (lambda (a b)
                     (string<
                      (substring a 0 -3)
                      (substring b 0 -3))))))
    ;; ensure that the `main-file' is placed at the beginning of list.
    (if (not (memq main-file res))
        res
      (cons main-file (delq main-file res)))))

(defun keg-function (subcommand)
  "Return function symbol from SUBCOMMAND."
  (intern (format "keg-command-%s" subcommand)))

(defun keg-usage (subcommand)
  "Return SUBCOMMAND CLI usage."
  (if (not (memq subcommand (keg-subcommands)))
      (error "Subcommand `%s' is not defined" subcommand)
    (let ((doc (documentation (keg-function subcommand))))
      (string-match "\\`.*$" doc)
      (match-string 0 doc))))

(defun keg-argument-usage (subcommand)
  "Return SUBCOMMAND CLI usage."
  (if (not (memq subcommand (keg-subcommands)))
      (error "Subcommand `%s' is not defined" subcommand)
    (let ((doc (documentation (keg-function subcommand))))
      (string-match "^USAGE: keg \\(.*\\)$" doc)
      (replace-regexp-in-string
       "‘\\(.*\\)’" "\\1"
       (replace-regexp-in-string
        "Emacs" "emacs"
        (match-string 1 doc)
        'case)))))

(defun keg-initialize ()
  "Set Emacs work in keg sandbox."
  (setq user-emacs-directory (keg-home-dir))
  (setq package-user-dir (keg-elpa-dir))
  (package-initialize)
  (add-to-list 'load-path (expand-file-name default-directory)))

(provide 'keg)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg.el ends here
