;;; init.el --- Initialization File -*- lexical-binding: t -*-

(let ((pkgs-path (expand-file-name "pkgs" user-emacs-directory)))
  (load (concat pkgs-path "/info/main"))
  (info-register-packages pkgs-path)
  (let ((info-main-path (info--library-spec->path '(info))))
    (puthash info-main-path '(info) info-loaded-files)))

(defvar user-dremacs-directory "~/.dremacs.d/")
(info-register-packages (expand-file-name "pkgs" user-dremacs-directory))
(let ((dremacs-init-file (expand-file-name "init" user-dremacs-directory)))
  (when (file-exists-p dremacs-init-file)
    (load dremacs-init-file)))
