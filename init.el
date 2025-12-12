;;; init.el --- Initialization File -*- lexical-binding: t -*-

(require 'gnus)

(let* ((scope-path (expand-file-name "pkgs" user-emacs-directory))
       (meta-path (expand-file-name "meta" scope-path))
       (meta-main-path (expand-file-name "main" meta-path)))
  (load meta-main-path)
  (meta-install-scope "system" scope-path))

(defvar user-dremacs-directory (expand-file-name ".dremacs.d" gnus-home-directory))
(unless (file-exists-p user-dremacs-directory)
  (copy-directory
   (file-name-concat (expand-file-name user-emacs-directory) ".example" ".dremacs.d")
   user-dremacs-directory))
(let ((init-path (file-name-concat user-dremacs-directory "init")))
  (load init-path t t))
