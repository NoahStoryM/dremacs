;;; main.el --- Racket-style info -*- lexical-binding: t; -*-

(require 'cl-lib)


(defvar info-import-paths '()
  "List of root directories where packages are located.")

(defvar info-collection-links (make-hash-table :test 'equal)
  "Mapping from collection name (string) to a list of root paths.")

(defvar info-installed-packages (make-hash-table :test 'equal)
  "Registry of all discovered packages.
Key: Package name (string, e.g., \"info\").
Value: Absolute path to the package root.")

(defvar info-loaded-files (make-hash-table :test 'equal)
  "Registry of loaded libraries to prevent circular or redundant loading.
Key: The library spec list (e.g., '(info info)).
Value: The absolute path of the loaded file. (e.d., \"~/.emacs.d/pkgs/info/info.el\")")

(defun info--link-collection (name root)
  "Register ROOT as a path for collection NAME.
If the collection is already registered, ROOT is prepended to the list,
allowing for shadowing behavior."
  (let ((root* (gethash name info-collection-links)))
    (puthash name (cons root root*) info-collection-links)))

(defun info--register-package (pkg-path)
  "Read `info.el' from PKG-PATH and register its collections.
This function expects `info.el' to define a variable containing a plist
with a `:collection' property."
  (let ((info-path (expand-file-name "info.el" pkg-path)))
    (when (file-exists-p info-path)
      (with-temp-buffer
        (insert-file-contents info-path)
        (goto-char (point-min))
        (let* ((form (read (current-buffer)))
               (var-symbol (eval form))
               (info (symbol-value var-symbol))
               (collection (plist-get info :collection))
               (dep* (plist-get info :deps)))
          (dolist (dep dep*)
            (unless (gethash dep info-installed-packages)
              (error "Package `%s' requires missing dependency: `%s'"
                     (file-name-nondirectory pkg-path) dep)))
          (cond
           ((or (eq collection "") (eq collection 'multi))
            (dolist (root (directory-files pkg-path t "^[^.]"))
              (when (file-directory-p root)
                (info--link-collection (file-name-nondirectory root) root))))
           ((eq collection 'use-pkg-name)
            (info--link-collection (file-name-nondirectory pkg-path) pkg-path))
           ((stringp collection)
            (info--link-collection collection pkg-path))
           (t
            (error "Invalid collection: %s" collection))))))))
(defun info-register-packages (pkgs-path)
  "Register all packages located within PKGS-PATH.
1. Scans subdirectories to update the `info-installed-packages` registry.
2. Registers collections and validates dependencies via `info--register-package`."
  (add-to-list 'info-import-paths pkgs-path)
  (let ((pkg-path* (directory-files pkgs-path t "^[^.]")))
    (dolist (pkg-path pkg-path*)
      (when (file-directory-p pkg-path)
        (let ((pkg-name (file-name-nondirectory pkg-path)))
          (puthash pkg-name pkg-path info-installed-packages))))
    (dolist (pkg-path pkg-path*)
      (when (file-directory-p pkg-path)
        (info--register-package pkg-path)))))

(defun info--library-spec->path (lib-spec)
  "Resolve LIB-SPEC (e.g., '(info info)) to an absolute file path.
Search order: .elc -> .el -> /main.elc -> /main.el"
  (let* ((name (symbol-name (car lib-spec)))
         (rel-path (mapconcat #'symbol-name (cdr lib-spec) "/"))
         (root* (gethash name info-collection-links)))
    (unless root*
      (error "Collection not registered: %s" name))
    (cl-block 'return
      (dolist (root root*)
        (let ((file-name (expand-file-name rel-path root)))
          (dolist (file-type '(".elc" ".el" "/main.elc" "/main.el"))
            (let ((file-path (concat file-name file-type)))
              (when (file-exists-p file-path)
                (cl-return-from 'return file-path))))))
      (error "Library not found: %s" lib-spec))))

(defun info-dynamic-import (lib-spec)
  "Load the module identified by LIB-SPEC if it hasn't been loaded yet."
  (let ((path (info--library-spec->path lib-spec)))
    (unless (gethash path info-loaded-files)
      (puthash path lib-spec info-loaded-files)
      (load path nil nil))))
(defmacro info-import (&rest lib-spec*)
  "Import modules using Racket-style syntax.
Example: (info-import (info) (info info))"
  `(progn
     ,@(mapcar (lambda (lib-spec)
                 `(info-dynamic-import ',lib-spec))
               lib-spec*)))
