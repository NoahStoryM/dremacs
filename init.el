;;; init.el --- Initialization File -*- lexical-binding: t -*-

(require 'gnus)

(let* ((scope-path (expand-file-name "pkgs" user-emacs-directory))
       (meta-path (expand-file-name "meta" scope-path))
       (meta-main-path (expand-file-name "main" meta-path)))
  (load meta-main-path)
  (meta-install-scope "system" scope-path))

(defvar user-dremacs-directory (expand-file-name ".dremacs.d" gnus-home-directory))
(defun dremacs--scaffold-file (path content)
  "Create file at PATH with CONTENT if it does not exist."
  (unless (file-exists-p path)
    (make-directory (file-name-directory path) t)
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) path))
    (message "Scaffolded new file: %s" path)))
(let* ((scope-path (file-name-concat user-dremacs-directory "pkgs"))
       (private-path (file-name-concat scope-path "private")))
  (dremacs--scaffold-file
   (file-name-concat user-dremacs-directory "init.el")
   (concat ";;; init.el --- User initialization file -*- lexical-binding: t -*-\n\n"
           "(meta-import (private))\n"))
  (dremacs--scaffold-file
   (file-name-concat private-path "meta.el")
   (concat ";;; meta.el --- Private package metadata -*- lexical-binding: t -*-\n\n"
           "(defmeta private-pkg-meta\n"
           "  (list :collection \"private\"\n"
           "        :pkg-desc \"User private package\"\n"
           "        :deps '(\"meta\")))\n\n"
           "(meta-export (private meta))\n"))
  (dremacs--scaffold-file
   (file-name-concat private-path "main.el")
   (concat ";;; main.el --- Private package entry -*- lexical-binding: t -*-\n\n"
           "(meta-export (private))\n")))
(let ((scope-path (file-name-concat user-dremacs-directory "pkgs")))
  (meta-install-scope "user" scope-path))
(load (file-name-concat user-dremacs-directory "init"))
