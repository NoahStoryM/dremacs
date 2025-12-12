;;; init.el --- Initialization File -*- lexical-binding: t -*-

(let ((scope-path (expand-file-name "pkgs" user-emacs-directory)))
  (load (concat scope-path "/meta/main"))
  (meta-install-scope "system" scope-path)
  (let ((meta-main-path (meta-library-spec->file-path '(meta))))
    (puthash meta-main-path "main" meta-installed-modules)))

(defvar user-dremacs-directory "~/.dremacs.d/")
(defun dremacs--scaffold-file (path content)
  "Create file at PATH with CONTENT if it does not exist."
  (unless (file-exists-p path)
    (make-directory (file-name-directory path) t)
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) path))
    (message "Scaffolded new file: %s" path)))
(let ((private-path (expand-file-name "pkgs/private" user-dremacs-directory)))
  (dremacs--scaffold-file
   (expand-file-name "init.el" user-dremacs-directory)
   (concat ";;; init.el --- User initialization file -*- lexical-binding: t -*-\n\n"
           "(meta-import (private))\n"))
  (dremacs--scaffold-file
   (concat private-path "/meta.el")
   (concat ";;; meta.el --- Private package metadata -*- lexical-binding: t -*-\n\n"
           "(defmeta private-pkg-meta\n"
           "  (list :collection \"private\"\n"
           "        :pkg-desc \"User private package\"\n"
           "        :deps '(\"meta\")))\n"))
  (dremacs--scaffold-file
   (concat private-path "/main.el")
   (concat ";;; main.el --- Private package entry -*- lexical-binding: t -*-\n")))
(let ((scope-path (expand-file-name "pkgs" user-dremacs-directory)))
  (meta-install-scope "user" scope-path))
(load (expand-file-name "init" user-dremacs-directory))
