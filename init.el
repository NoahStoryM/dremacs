;;; init.el --- System Initialization File -*- lexical-binding: t -*-

(let* ((scope-path (expand-file-name "pkgs" user-emacs-directory))
       (meta-path (expand-file-name "meta" scope-path))
       (meta-main-path (expand-file-name "main" meta-path)))
  (load meta-main-path nil t)
  (meta-install-scope "system" scope-path))

(let ((init-path (locate-file "init" (list user-dremacs-directory) load-suffixes)))
  (when (file-exists-p init-path)
    (load init-path nil t)))
