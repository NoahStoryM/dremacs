;;; main.el --- Racket-style collections for Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Scopes contain packages, packages provide collections, collections
;; contain modules.  A module is identified by a library spec such as
;; `(private layers default)', which resolves to a file under one of the
;; roots registered for the `private' collection.
;;
;; Meta organizes where modules live and how they are loaded.  It does
;; not isolate identifiers: definitions in different modules still share
;; Emacs' global namespace, so modules should keep using name prefixes.
;;
;; Invariants:
;; - Resolution never consults `load-path'; every root is tracked in
;;   `meta-installed-collections'.
;; - `meta-modules' alone decides whether a module is loaded.  Features
;;   are provided only as an after-the-fact signal for `eval-after-load',
;;   and they live under a `meta:' prefix so they never collide with
;;   ordinary Emacs features such as `dired'.

;;; Code:

(require 'seq)

;;;; Registries

(defvar meta-installed-scopes (make-hash-table :test 'equal)
  "Registry of installed scopes.
Key: scope name (e.g. \"system\", \"user\").
Value: absolute path of the scope directory.")

(defvar meta-installed-packages (make-hash-table :test 'equal)
  "Registry of all installed packages across all scopes.
Key: package name (string).
Value: absolute path of the package directory.")

(defvar meta-installed-collections (make-hash-table :test 'equal)
  "Registry of collection roots.
Key: collection name (string).
Value: list of absolute paths, highest priority first.")

(defvar meta-modules (make-hash-table :test 'equal)
  "Load state of modules.
Key: module file name without its .el/.elc suffix.
Value: `loading' while the file is being loaded, `loaded' once it has
finished without error.  A module that signals an error is removed, so
it can be imported again after it is fixed.")

(defvar meta--loading nil
  "Stack of modules currently being loaded, innermost first.
Only used to describe cycles in error messages.")

;;;; Metadata

(defun meta--read-metadata (package-name package-path)
  "Read the metadata plist of PACKAGE-NAME from PACKAGE-PATH/metadata.el.
The file holds a single plist and is read as data, never evaluated."
  (let ((file (expand-file-name "metadata.el" package-path)))
    (unless (file-readable-p file)
      (error "Package `%s' has no metadata.el" package-name))
    (let ((info (with-temp-buffer
                  (insert-file-contents file)
                  (read (current-buffer)))))
      (unless (and (plistp info) (keywordp (car-safe info)))
        (error "Package `%s': metadata.el must contain a plist" package-name))
      info)))

;;;; Installation

(defun meta--subdirectories (path)
  "Return the non-hidden subdirectories of PATH as absolute paths."
  (seq-filter #'file-directory-p (directory-files path t "\\`[^.]")))

(defun meta-install-scope (scope-name scope-path)
  "Install the scope SCOPE-NAME rooted at SCOPE-PATH.
This runs in two passes so that packages may depend on each other
regardless of directory order:
1. Discovery: record every package in `meta-installed-packages'.
2. Installation: read each package's metadata and register its
   collections via `meta-install-package'.
Installing the same scope again is harmless."
  (let* ((scope-path (expand-file-name scope-path))
         (package-path* (meta--subdirectories scope-path)))
    (puthash scope-name scope-path meta-installed-scopes)
    (dolist (package-path package-path*)
      (puthash (file-name-nondirectory package-path) package-path
               meta-installed-packages))
    (dolist (package-path package-path*)
      (meta-install-package (file-name-nondirectory package-path) package-path))))

(defun meta-install-package (package-name package-path)
  "Read the metadata of PACKAGE-NAME at PACKAGE-PATH and register its collections.
Every dependency listed under :deps must already be a known package."
  (let* ((info (meta--read-metadata package-name package-path))
         (collection (plist-get info :collection)))
    (dolist (dep (plist-get info :deps))
      (unless (gethash dep meta-installed-packages)
        (error "Package `%s' requires missing dependency: `%s'" package-name dep)))
    (cond
     ((or (null collection) (eq collection 'use-pkg-name))
      (meta-install-collection package-name package-path))
     ((or (equal collection "") (eq collection 'multi))
      (dolist (collection-path (meta--subdirectories package-path))
        (meta-install-collection (file-name-nondirectory collection-path)
                                 collection-path)))
     ((stringp collection)
      (meta-install-collection collection package-path))
     (t
      (error "Package `%s': invalid collection %S" package-name collection)))))

(defun meta-install-collection (collection-name collection-path)
  "Register COLLECTION-PATH as a root of COLLECTION-NAME.
Roots registered later shadow earlier ones.  Registering a root that is
already known leaves its priority unchanged."
  (let ((path (directory-file-name (expand-file-name collection-path)))
        (roots (gethash collection-name meta-installed-collections)))
    (unless (member path roots)
      (puthash collection-name (cons path roots) meta-installed-collections))))

;;;; Library specs

(defun meta-library-spec->file-path (library-spec)
  "Resolve LIBRARY-SPEC (e.g. (meta) or (private layers default)) to a file.
The spec (C X ... Y) names X/.../Y.el or, failing that, X/.../Y/main.el;
the spec (C) names main.el.  Roots of collection C are searched in
priority order, and both forms are tried in each root before moving to
the next, so a higher-priority root always wins."
  (let* ((collection-name (symbol-name (car library-spec)))
         (roots (or (gethash collection-name meta-installed-collections)
                    (error "Collection not registered: %s" collection-name)))
         (module-path (mapconcat #'symbol-name (cdr library-spec) "/"))
         (candidates (if (cdr library-spec)
                         (list module-path (file-name-concat module-path "main"))
                       (list "main"))))
    (or (seq-some (lambda (root)
                    (seq-some (lambda (candidate)
                                (locate-file candidate (list root) load-suffixes))
                              candidates))
                  roots)
        (error "Library not found: %S" library-spec))))

(defun meta-library-spec->feature (library-spec)
  "Return the feature Meta provides for LIBRARY-SPEC.
For example (private layers) => `meta:private/layers'.  A trailing
`main' is dropped, so (C main) and (C) name the same feature."
  (let ((spec (if (and (cdr library-spec) (eq (car (last library-spec)) 'main))
                  (butlast library-spec)
                library-spec)))
    (intern (concat "meta:" (mapconcat #'symbol-name spec "/")))))

(defun meta--module-key (file)
  "Return the key of FILE in `meta-modules': FILE without .el/.elc."
  (replace-regexp-in-string "\\.elc?\\'" "" file))

;;;; Import

(defun meta--describe-cycle (key)
  "Describe the import cycle that reaches KEY again."
  (mapconcat #'abbreviate-file-name
             (append (member key (reverse meta--loading)) (list key))
             " -> "))

(defun meta-dynamic-import (library-spec)
  "Load the module named by LIBRARY-SPEC unless it is already loaded.
Return the module's feature.  Signal an error on cyclic imports."
  (let* ((file (meta-library-spec->file-path library-spec))
         (key (meta--module-key file))
         (feature (meta-library-spec->feature library-spec)))
    (pcase (gethash key meta-modules)
      ('loaded nil)
      ('loading (error "Cycle in loading: %s" (meta--describe-cycle key)))
      (_
       (puthash key 'loading meta-modules)
       (let ((done nil))
         (unwind-protect
             (let ((meta--loading (cons key meta--loading)))
               (load key nil t nil t)
               (setq done t))
           (if done
               (puthash key 'loaded meta-modules)
             (remhash key meta-modules))))))
    ;; Provide only once: every `provide' reruns the `eval-after-load'
    ;; callbacks of the feature.  This is a notification only; whether
    ;; the module is loaded is still decided by `meta-modules'.
    (unless (featurep feature)
      (provide feature))
    feature))

(defmacro meta-import (&rest library-spec*)
  "Import modules.
Example: (meta-import (meta) (private layers default))"
  `(progn
     ,@(mapcar (lambda (library-spec) `(meta-dynamic-import ',library-spec))
               library-spec*)))

;;;; Autoload

(defconst meta--interactive-call (make-symbol "meta-interactive-call")
  "Marker argument passed by the interactive spec of autoload stubs.")

(defun meta-dynamic-auto-import (function library-spec &optional docstring interactive)
  "Define FUNCTION as a stub that imports LIBRARY-SPEC on its first call.
Like `autoload', but the module is loaded through `meta-dynamic-import',
so importing the same module later does not load it a second time.
If loading fails, the stub is put back, so the next call retries even if
the module defined FUNCTION before failing.  DOCSTRING belongs to the
stub and is replaced along with it.  If INTERACTIVE is non-nil the stub
is a command.  Does nothing when FUNCTION is already defined."
  (meta-library-spec->file-path library-spec) ; report bad specs early
  (unless (fboundp function)
    (let* ((stub nil)
           (run (lambda (args)
                  (condition-case err
                      (meta-dynamic-import library-spec)
                    (t (fset function stub)
                       (signal (car err) (cdr err))))
                  (when (eq (symbol-function function) stub)
                    (error "Module %S did not define `%s'" library-spec function))
                  (if (and args (eq (car args) meta--interactive-call))
                      (call-interactively function)
                    (apply function args)))))
      ;; Build the stub with `eval' so DOCSTRING lives in the function
      ;; object itself; a `function-documentation' property (what
      ;; `defalias' would set) would outlive the stub and shadow the real
      ;; docstring.
      (setq stub (eval `(lambda (&rest args)
                          ,@(and docstring (list docstring))
                          ,@(and interactive '((interactive (list meta--interactive-call))))
                          (funcall ',run args))
                       t))
      (defalias function stub))))

;; This file is loaded with a plain `load' by the system init.el.
(when load-file-name
  (puthash (meta--module-key load-file-name) 'loaded meta-modules))
(provide 'meta:meta)
(provide 'meta)

;;; main.el ends here
