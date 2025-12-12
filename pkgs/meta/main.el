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
  (setq meta--cache:info nil)
  (load (expand-file-name "info" package-path) nil t)
  (unless meta--cache:info
    (error "Package `%s' did not define metadata via `definfo'" package-name))
  (let ((collection (plist-get meta--cache:info :collection))
        (dep* (plist-get meta--cache:info :deps)))
    (setq meta--cache:info nil)
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
(defun meta-install-module (feature file-path)
  "Instantiate a module (require feature) if not already installed."
  (unless (gethash file-path meta-installed-modules)
    (puthash file-path feature meta-installed-modules)
    (require feature file-path)))

(defvar meta--cache:info nil
  "Temporary storage for the most recently loaded package metadata.
This variable is updated by the `definfo' macro and consumed by
`meta-install-package'. It acts as a bridge between the loaded file
and the package manager.")
(defmacro definfo (symbol value &optional docstring)
  "Define a package metadata variable and register it for installation."
  `(progn (defvar ,symbol ,value ,docstring)
          (setq meta--cache:info ,symbol)))

(defun meta-library-spec->file-path (library-spec)
  "Resolve a library spec (e.g. '(meta main)) to an absolute path."
  (let* ((collection-name (symbol-name (car library-spec)))
         (collection-path* (gethash collection-name meta-installed-collections)))
    (unless collection-path*
      (error "Collection not registered: %s" collection-name))
    (let* ((rel-path (cdr library-spec))
           (module-path (if rel-path (apply #'file-name-concat (mapcar #'symbol-name rel-path)) ""))
           (file-path (locate-file module-path collection-path* load-suffixes)))
      (or file-path
          (let* ((module-path (file-name-concat module-path "main"))
                 (file-path (locate-file module-path collection-path* load-suffixes)))
            (unless file-path
              (error "Library not found: %s" library-spec))
            file-path)))))
(defun meta-library-spec->feature (library-spec)
  "Transform a library spec (e.g. '(library meta)) to a feature (e.g. 'library/meta)."
  (intern (mapconcat #'symbol-name library-spec "/")))

(defun meta-dynamic-import (library-spec)
  "Resolve and install the module specified by LIBRARY-SPEC."
  (let ((file-path (meta-library-spec->file-path library-spec))
        (feature (meta-library-spec->feature library-spec)))
    (meta-install-module feature file-path)))
(defmacro meta-import (&rest library-spec*)
  "Import modules.
Example: (meta-import (meta) (private))"
  `(progn
     ,@(mapcar (lambda (library-spec)
                 `(meta-dynamic-import ',library-spec))
               library-spec*)))

(defun meta-dynamic-auto-import (function library-spec &optional docstring interactive type)
  "Register native autoloads for FUNCTIONS in LIBRARY-SPEC."
  (let ((file-path (meta-library-spec->file-path library-spec))
        (feature (meta-library-spec->feature library-spec)))
    (autoload function file-path docstring interactive type)))

(defvar meta--pending-provides (make-hash-table :test 'equal)
  "Registry of features provided by files currently being loaded.
Key: Absolute file path (string).
Value: Feature symbol.")
(defun meta-dynamic-export (library-spec)
  "Export the current file as LIBRARY-SPEC."
  (let ((feature (meta-library-spec->feature library-spec)))
    (provide feature)
    (when load-file-name
      (puthash load-file-name feature meta--pending-provides))))
(defmacro meta-export (library-spec)
  "Declare the current file's module identity."
  `(meta-dynamic-export ',library-spec))

(defun meta--on-file-load (file-path)
  "Hook: Run after file load. Check if it opted-in via meta-provide."
  (let ((feature (gethash file-path meta--pending-provides)))
    (when feature
      (puthash file-path feature meta-installed-modules)
      (remhash file-path meta--pending-provides))))
(add-hook 'after-load-functions #'meta--on-file-load)

(meta-export (meta))
