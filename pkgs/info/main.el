;;; main.el --- Racket-style info -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar info-installed-scopes (make-hash-table :test 'equal)
  "Registry of installed scopes.
Key: Scope name (e.g., \"system\", \"user\").
Value: Absolute path to the scope directory.")
(defun info-install-scope (scope-name scope-path)
  "Install a scope by registering all valid packages within it.
This runs in two passes:
1. Discovery: Map all package names to paths in `info-installed-packages'.
2. Installation: Parse `info.el' and register collections via `info-install-package'."
  (puthash scope-name scope-path info-installed-scopes)
  (let ((package-path* (directory-files scope-path t "^[^.]")))
    (dolist (package-path package-path*)
      (when (file-directory-p package-path)
        (let ((package-name (file-name-nondirectory package-path)))
          (puthash package-name package-path info-installed-packages))))
    (dolist (package-path package-path*)
      (when (file-directory-p package-path)
        (let ((package-name (file-name-nondirectory package-path)))
          (info-install-package package-name package-path))))))

(defvar info-installed-packages (make-hash-table :test 'equal)
  "Registry of all installed packages across all scopes.
Key: Package name (string).
Value: Absolute path to the package directory.")
(defun info-install-package (package-name package-path)
  "Read metadata from `info.el' and register collections.
Validates dependencies against `info-installed-packages'."
  (setq info--cache:info nil)
  (load (expand-file-name "info" package-path) nil t)
  (unless info--cache:info
    (error "Package `%s' did not define metadata via `definfo'" package-name))
  (let ((collection (plist-get info--cache:info :collection))
        (dep* (plist-get info--cache:info :deps)))
    (setq info--cache:info nil)
    (dolist (dep dep*)
      (unless (gethash dep info-installed-packages)
        (error "Package `%s' requires missing dependency: `%s'"
               (file-name-nondirectory package-path) dep)))
    (cond
     ((or (not collection) (eq collection 'use-pkg-name))
      (info-install-collection package-name package-path))
     ((or (eq collection "") (eq collection 'multi))
      (dolist (collection-path (directory-files package-path t "^[^.]"))
        (when (file-directory-p collection-path)
          (let ((collection-name (file-name-nondirectory collection-path)))
            (info-install-collection collection-name collection-path)))))
     ((stringp collection)
      (info-install-collection collection package-path))
     (t
      (error "Invalid collection: %s" collection)))))

(defvar info-installed-collections (make-hash-table :test 'equal)
  "Registry of collection roots.
Key: Collection name (string).
Value: List of absolute paths (shadowing supported).")
(defun info-install-collection (collection-name collection-path)
  "Register a root path for a collection."
  (let ((collection-path* (gethash collection-name info-installed-collections)))
    (puthash collection-name (cons collection-path collection-path*) info-installed-collections)
    (add-to-list 'load-path collection-path)))

(defvar info-installed-modules (make-hash-table :test 'equal)
  "Registry of instantiated modules to prevent re-instantiation.
Key: Absolute file path.
Value: Module name (string).")
(defun info-install-module (module-name file-path)
  "Instantiate a module (load file) if not already installed."
  (unless (gethash file-path info-installed-modules)
    (puthash file-path module-name info-installed-modules)
    (load file-path nil nil)))

(defvar info--cache:info nil
  "Temporary storage for the most recently loaded package metadata.
This variable is updated by the `definfo' macro and consumed by
`info-install-package'. It acts as a bridge between the loaded file
and the package manager.")
(defmacro definfo (symbol value docstring)
  "Define a package metadata variable and register it for installation."
  `(progn (defvar ,symbol ,value ,docstring)
          (setq info--cache:info ,symbol)))

(defun info-library-spec->file-path (library-spec)
  "Resolve a library spec (e.g. '(info main)) to an absolute path."
  (let* ((collection-name (symbol-name (car library-spec)))
         (module-path (mapconcat #'symbol-name (cdr library-spec) "/"))
         (collection-path* (gethash collection-name info-installed-collections)))
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

(defun info-dynamic-import (library-spec)
  "Resolve and install the module specified by LIBRARY-SPEC."
  (let* ((file-path (info-library-spec->file-path library-spec))
         (module-name (file-name-base file-path)))
    (info-install-module module-name file-path)))
(defmacro info-import (&rest library-spec*)
  "Import modules.
Example: (info-import (info) (private))"
  `(progn
     ,@(mapcar (lambda (library-spec)
                 `(info-dynamic-import ',library-spec))
               library-spec*)))
