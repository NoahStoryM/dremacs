;;; early-init.el --- System Bootstrapping File -*- lexical-binding: t -*-

(defvar user-dremacs-directory
  (file-name-concat (file-name-directory (directory-file-name user-emacs-directory))
                    ".dremacs.d/")
  "Root directory for the user's dremacs configuration.")
(unless (file-exists-p user-dremacs-directory)
  (let ((template-dremacs-directory
         (file-name-concat user-emacs-directory ".template" ".dremacs.d")))
    (copy-directory template-dremacs-directory user-dremacs-directory)))

(let ((early-init-path (locate-file "early-init" (list user-dremacs-directory) load-suffixes)))
  (when early-init-path
    (load early-init-path t t)))
