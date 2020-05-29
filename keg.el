;;; keg.el --- Modern Elisp package development system  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
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

(require 'lisp-mnt)
(require 'subr-x)
(require 'package)

(defgroup keg nil
  "Modern Elisp package development system."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/keg.el"))


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
        sources devs packages)
    (when path
      (dolist (elm (read (with-temp-buffer
                           (insert-file-contents path)
                           (format "(%s)" (buffer-string)))))
        (let ((op (car elm))
              (args (cdr elm)))
          (cond
           ((eq 'sources op)
            (dolist (elm args) (push elm sources)))
           ((eq 'dev-dependencies op)
            (dolist (elm args) (push elm devs)))
           ((eq 'package op)
            (push args packages)))))
      `((sources . ,(nreverse (delete-dups sources)))
        (devs . ,(nreverse (delete-dups devs)))
        (packages . ,(nreverse (delete-dups packages)))))))

(defun keg-file-read-section (section)
  "Return SECTION value from Keg file."
  (keg--alist-get section (keg-file-read)))


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

(defun keg-build--expand-source-file-list (recipe dir)
  "Resolve source file from RECIPE in DIR.
See `package-build--expand-source-file-list' from MELPA package-build."
  (mapcar 'car
          (keg-build--expand-file-specs
           dir
           (keg-build--config-file-list recipe))))

(defun keg-build--get-dependency-from-elisp-file (file)
  "Get package dependency from Package-Require header from FILE.
Duplicate requires are resolved by more restrictive."
  (if (not (file-readable-p file))
      (warn "File %s is missing")
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
    (push `(keg--devs . ,(mapcar (lambda (elm) `(,elm ,(version-to-list "0.0.1"))) devs)) ret)
    (nreverse ret)))

(defun keg-build--package-archives ()
  "Return appropriate `packages-archives' using Keg sources value."
  (let ((urls '((gnu . "https://elpa.gnu.org/packages/")
                (org . "https://orgmode.org/elpa/")
                (melpa . "https://melpa.org/packages/")
                (celpa . "https://celpa.conao3.com/packages/"))))
    (mapcar
     (lambda (elm)
       (let ((url (keg--alist-get elm urls)))
         (if (not url)
             (error "Source %s is unknown" elm)
           `(,(symbol-name elm) . ,url))))
     (keg-file-read-section 'sources))))

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


;;; Functions

(defun keg--princ (&optional str &rest args)
  "Do `princ' STR with format ARGS and put \n."
  (when str (princ (apply #'format str args)))
  (princ "\n"))

(defun keg--indent (width str)
  "Add indent of WIDTH for STR each lines."
  (replace-regexp-in-string "^" (make-string width ?\s) str))

(defun keg--alist-get (key alist &optional default _remove testfn)
  "Find the first element of ALIST whose `car' equals KEY and return its `cdr'.
If KEY is not found in ALIST, return DEFAULT.
Equality with KEY is tested by TESTFN, defaulting to `eq'.

This function is `alist-get' polifill for Emacs < 25.1."
  (let ((x (if (not testfn)
               (assq key alist)
             (assoc key alist testfn))))
    (if x (cdr x) default)))

(defun keg-subcommands ()
  "Return keg subcommands."
  (let (res)
    (mapatoms
     (lambda (elm)
       (when (and (fboundp elm)
                  (string-prefix-p "keg-main-" (symbol-name elm)))
         (push (intern (replace-regexp-in-string "^keg-main-" "" (symbol-name elm))) res))))
    (sort res (lambda (a b) (string< (symbol-name a) (symbol-name b))))))


;;; Main

(defun keg-main-help ()
  "Show this help."
  (keg--princ
   "USAGE: keg [SUBCOMMAND] [OPTIONS...]

Modern Elisp package development system

SUBCOMMANDS:")
  (keg--princ
   (mapconcat
    (lambda (elm)
      (format
       " %s\n%s"
       elm
       (keg--indent 5 (documentation (intern (format "keg-main-%s" elm))))))
    (keg-subcommands)
    "\n")))

(defun keg-main-version ()
  "Show `keg' version."
  (keg--princ
   (format "Keg %s running on Emacs %s"
           (eval-when-compile
             (lm-version (or load-file-name
                             byte-compile-current-file)))
           emacs-version)))

(defun keg-main-init ()
  "Create Keg template file."
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

(defun keg-main-install ()
  "Install dependencies in .keg folder."
  (keg--princ "Install dependencies")
  (let ((reqinfo (keg-build--get-dependency-from-keg-file)))
    (dolist (info (keg-file-read-section 'packages))
      (let* ((name (car info))
             (_alist (cdr info))
             (reqs (keg--alist-get name reqinfo)))
        (keg--princ (format " Package: %s" name))
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
  (keg-build--resolve-dependency))

(defun keg-main-info ()
  "Show this package information."
  (keg--princ "Keg file parsed")
  (keg--princ (pp-to-string (keg-file-read))))

(defun keg-main-load-path ()
  "Return `load-path' in the form of PATH."
  (keg--princ (mapconcat #'identity (mapcar #'shell-quote-argument load-path) ":")))

(defun keg-main-debug ()
  "Show debug information."
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

(defun keg-main ()
  "Init `keg' and exec subcommand."
  (let* ((op (car command-line-args-left))
         (args (cdr command-line-args-left))
         (user-emacs-directory
          (expand-file-name (format ".keg/%s" emacs-version)))
         (package-user-dir (locate-user-emacs-file "elpa")))
    (make-directory user-emacs-directory 'parent)
    (package-initialize)
    (cond
     ((eq nil op)
      (keg-main-install))
     ((memq (intern op) (keg-subcommands))
      (apply (intern (format "keg-main-%s" op)) args))
     (t
      (keg--princ (format "Subcommand `%s' is missing" op))
      (keg--princ)
      (keg-main-help)
      (kill-emacs 1)))))

(provide 'keg)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; keg.el ends here
