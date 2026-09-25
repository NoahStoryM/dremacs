;;; main.el --- Racket-style collections for Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Scopes contain packages, packages provide collections, collections
;; contain modules.  A module is identified by a library spec such as
;; `(private layers default)', which resolves to a file under one of the
;; roots registered for the `private' collection.
;;
;; Unlike `require', resolution never consults `load-path': every root is
;; tracked explicitly in `meta-installed-collections'.

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
Value: list of absolute paths; earlier entries shadow later ones.")

(defvar meta-instantiated-modules (make-hash-table :test 'equal)
  "Modules that have been loaded successfully.
Key: absolute file path.
Value: feature symbol.
A file is only recorded here after it has finished loading without
error, so a module that failed can be imported again after it is fixed.")

(defvar meta--loading nil
  "Stack of module files currently being instantiated, innermost first.
Used to report cyclic imports instead of silently ignoring them.")

;;;; Metadata

(defun meta--read-metadata (package-name package-path)
  "Read the metadata plist of PACKAGE-NAME from PACKAGE-PATH/metadata.el.
The file holds a single plist and is read, not evaluated.  The legacy
form (definfo SYMBOL VALUE [DOC]) is still accepted."
  (let ((file (expand-file-name "metadata.el" package-path)))
    (unless (file-readable-p file)
      (error "Package `%s' has no metadata.el" package-name))
    (let ((form (with-temp-buffer
                  (insert-file-contents file)
                  (read (current-buffer)))))
      (pcase form
        (`(definfo ,_ ,value . ,_) (eval value t))
        ((and (pred plistp) (guard (keywordp (car-safe form)))) form)
        (_ (error "Package `%s': metadata.el must contain a plist" package-name))))))

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
   collections via `meta-install-package'."
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
Roots registered later shadow earlier ones."
  (push collection-path (gethash collection-name meta-installed-collections)))

;;;; Library specs

(defun meta-library-spec->file-path (library-spec)
  "Resolve LIBRARY-SPEC (e.g. (meta) or (private layers default)) to a file.
The spec (C X ... Y) resolves to X/.../Y.el under a root of collection C,
falling back to X/.../Y/main.el."
  (let* ((collection-name (symbol-name (car library-spec)))
         (roots (or (gethash collection-name meta-installed-collections)
                    (error "Collection not registered: %s" collection-name)))
         (module-path (mapconcat #'symbol-name (cdr library-spec) "/")))
    (or (and (cdr library-spec)
             (locate-file module-path roots load-suffixes))
        (locate-file (if (cdr library-spec)
                         (file-name-concat module-path "main")
                       "main")
                     roots load-suffixes)
        (error "Library not found: %S" library-spec))))

(defun meta-library-spec->feature (library-spec)
  "Return the feature for LIBRARY-SPEC, e.g. (private layers) => private/layers."
  (intern (mapconcat #'symbol-name library-spec "/")))

;;;; Import / export

(defun meta-dynamic-import (library-spec)
  "Load the module named by LIBRARY-SPEC unless it is already loaded.
Signal an error on cyclic imports.  The module's feature is provided
only after the file has loaded successfully."
  (let ((feature (meta-library-spec->feature library-spec)))
    (unless (featurep feature)
      (let ((file (meta-library-spec->file-path library-spec)))
        (unless (gethash file meta-instantiated-modules)
          (when (member file meta--loading)
            (error "Cycle in loading: %s"
                   (mapconcat #'abbreviate-file-name
                              (reverse (cons file meta--loading)) " -> ")))
          (let ((meta--loading (cons file meta--loading)))
            (load file nil t t))
          (puthash file feature meta-instantiated-modules))
        (provide feature)))
    feature))

(defmacro meta-import (&rest library-spec*)
  "Import modules.
Example: (meta-import (meta) (private layers default))"
  `(progn
     ,@(mapcar (lambda (library-spec) `(meta-dynamic-import ',library-spec))
               library-spec*)))

(defun meta-dynamic-auto-import (function library-spec &optional docstring interactive type)
  "Autoload FUNCTION from the module named by LIBRARY-SPEC."
  (autoload function (meta-library-spec->file-path library-spec)
    docstring interactive type))

(defun meta-dynamic-export (library-spec)
  "Provide the feature of LIBRARY-SPEC.
Optional: `meta-import' already provides the feature once the file has
loaded, so modules only need this when they are loaded by other means."
  (provide (meta-library-spec->feature library-spec)))

(defmacro meta-export (library-spec)
  "Declare the current file's module identity.  See `meta-dynamic-export'."
  `(meta-dynamic-export ',library-spec))

;; `meta' itself is loaded with a plain `load', so record it by hand.
(when load-file-name
  (puthash load-file-name 'meta meta-instantiated-modules))
(meta-export (meta))

;;; main.el ends here
