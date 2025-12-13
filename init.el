;;; init.el --- System Initialization File -*- lexical-binding: t -*-

(when (< emacs-major-version 29)
  (error "DrEmacs only works with Emacs 29 and newer"))

(let* ((scope-path (expand-file-name "pkgs" user-emacs-directory))
       (meta-path (expand-file-name "meta" scope-path))
       (meta-main-path (expand-file-name "main" meta-path)))
  (load meta-main-path nil t)
  (meta-install-scope "system" scope-path))

(let ((init-path (locate-file "init" (list user-dremacs-directory) load-suffixes)))
  (when init-path
    (load init-path t t)))
