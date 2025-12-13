;;; early-init.el --- System Bootstrapping File -*- lexical-binding: t -*-

(defvar user-dremacs-directory
  (file-name-concat (file-name-directory (directory-file-name user-emacs-directory))
                    ".dremacs.d/"))
(unless (file-exists-p user-dremacs-directory)
  (copy-directory
   (file-name-concat (expand-file-name user-emacs-directory)
                     ".example" ".dremacs.d")
   user-dremacs-directory))

(let ((early-init-path (locate-file "early-init" (list user-dremacs-directory) load-suffixes)))
  (when (file-exists-p early-init-path)
    (load early-init-path nil t)))
