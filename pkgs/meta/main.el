;;; main.el --- Racket-style info -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar meta-installed-scopes (make-hash-table :test 'equal)
  "Registry of installed scopes.
Key: Scope name (e.g., \"system\", \"user\").
Value: Absolute path to the scope directory.")
(defun meta-install-scope (scope-name scope-path)
  "Install a scope by registering all valid packages within it.
This runs in two passes:
1. Discovery: Map all package names to paths in `meta-installed-packages'.
2. Installation: Parse `meta.el' and register collections via `meta-install-package'."
  (puthash scope-name scope-path meta-installed-scopes)
  (let ((package-path* (directory-files scope-path t "^[^.]")))
    (dolist (package-path package-path*)
      (when (file-directory-p package-path)
        (let ((package-name (file-name-nondirectory package-path)))
          (puthash package-name package-path meta-installed-packages))))
    (dolist (package-path package-path*)
      (when (file-directory-p package-path)
        (let ((package-name (file-name-nondirectory package-path)))
          (meta-install-package package-name package-path))))))

(defvar meta-installed-packages (make-hash-table :test 'equal)
  "Registry of all installed packages across all scopes.
Key: Package name (string).
Value: Absolute path to the package directory.")
(defun meta-install-package (package-name package-path)
  "Read metadata from `meta.el' and register collections.
Validates dependencies against `meta-installed-packages'."
  (setq meta--cache:meta nil)
  (load (expand-file-name "meta" package-path) nil t)
  (unless meta--cache:meta
    (error "Package `%s' did not define metadata via `defmeta'" package-name))
  (let ((collection (plist-get meta--cache:meta :collection))
        (dep* (plist-get meta--cache:meta :deps)))
    (setq meta--cache:meta nil)
    (dolist (dep dep*)
      (unless (gethash dep meta-installed-packages)
        (error "Package `%s' requires missing dependency: `%s'"
               (file-name-nondirectory package-path) dep)))
    (cond
     ((or (not collection) (eq collection 'use-pkg-name))
      (meta-install-collection package-name package-path))
     ((or (eq collection "") (eq collection 'multi))
      (dolist (collection-path (directory-files package-path t "^[^.]"))
        (when (file-directory-p collection-path)
          (let ((collection-name (file-name-nondirectory collection-path)))
            (meta-install-collection collection-name collection-path)))))
     ((stringp collection)
      (meta-install-collection collection package-path))
     (t
      (error "Invalid collection: %s" collection)))))

(defvar meta-installed-collections (make-hash-table :test 'equal)
  "Registry of collection roots.
Key: Collection name (string).
Value: List of absolute paths (shadowing supported).")
(defun meta-install-collection (collection-name collection-path)
  "Register a root path for a collection."
  (let ((collection-path* (gethash collection-name meta-installed-collections)))
    (puthash collection-name (cons collection-path collection-path*) meta-installed-collections)
    (add-to-list 'load-path collection-path)))

(defvar meta-installed-modules (make-hash-table :test 'equal)
  "Registry of instantiated modules to prevent re-instantiation.
Key: Absolute file path.
Value: Module name (string).")
(defun meta-install-module (module-name file-path)
  "Instantiate a module (load file) if not already installed."
  (unless (gethash file-path meta-installed-modules)
    (puthash file-path module-name meta-installed-modules)
    (load file-path nil nil)))

(defvar meta--cache:meta nil
  "Temporary storage for the most recently loaded package metadata.
This variable is updated by the `defmeta' macro and consumed by
`meta-install-package'. It acts as a bridge between the loaded file
and the package manager.")
(defmacro defmeta (symbol value docstring)
  "Define a package metadata variable and register it for installation."
  `(progn (defvar ,symbol ,value ,docstring)
          (setq meta--cache:meta ,symbol)))

(defun meta-library-spec->file-path (library-spec)
  "Resolve a library spec (e.g. '(meta main)) to an absolute path."
  (let* ((collection-name (symbol-name (car library-spec)))
         (module-path (mapconcat #'symbol-name (cdr library-spec) "/"))
         (collection-path* (gethash collection-name meta-installed-collections)))
    (unless collection-path*
      (error "Collection not registered: %s" collection-name))
    (cl-block 'return
      (dolist (collection-path collection-path*)
        (let ((file-name (expand-file-name module-path collection-path)))
          (dolist (file-type '(".elc" ".el" "/main.elc" "/main.el"))
            (let ((file-path (concat file-name file-type)))
              (when (file-exists-p file-path)
                (cl-return-from 'return file-path))))))
      (error "Library not found: %s" library-spec))))

(defun meta-dynamic-import (library-spec)
  "Resolve and install the module specified by LIBRARY-SPEC."
  (let* ((file-path (meta-library-spec->file-path library-spec))
         (module-name (file-name-base file-path)))
    (meta-install-module module-name file-path)))
(defmacro meta-import (&rest library-spec*)
  "Import modules.
Example: (meta-import (meta) (private))"
  `(progn
     ,@(mapcar (lambda (library-spec)
                 `(meta-dynamic-import ',library-spec))
               library-spec*)))
