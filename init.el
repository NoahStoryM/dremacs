;;; init.el --- Initialization File -*- lexical-binding: t -*-

(let ((pkgs-path (expand-file-name "pkgs" user-emacs-directory)))
  (load (concat pkgs-path "/info/main"))
  (info-register-packages pkgs-path)
  (let ((info-main-path (info--library-spec->path '(info))))
    (puthash info-main-path '(info) info-loaded-files)))

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
           "(info-import (private))\n"))

  (dremacs--scaffold-file
   (concat private-path "/info.el")
   (concat ";;; info.el --- Private package metadata -*- lexical-binding: t -*-\n\n"
           "(defvar private-pkg-info\n"
           "  (list :collection \"private\"\n"
           "        :pkg-desc \"User private package\"\n"
           "        :deps '(\"info\")))\n"))

  (dremacs--scaffold-file
   (concat private-path "/main.el")
   (concat ";;; main.el --- Private package entry -*- lexical-binding: t -*-\n")))

(info-register-packages (expand-file-name "pkgs" user-dremacs-directory))
(let ((dremacs-init-file (expand-file-name "init" user-dremacs-directory)))
  (when (file-exists-p (concat dremacs-init-file ".el"))
    (load dremacs-init-file)))
