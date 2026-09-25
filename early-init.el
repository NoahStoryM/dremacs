;;; early-init.el --- System Bootstrapping File -*- lexical-binding: t -*-

(defconst dremacs-directory (file-name-as-directory (expand-file-name user-emacs-directory))
  "Root directory of the DrEmacs system (this repository).
Captured before the user's early-init runs, so the user is free to
change `user-emacs-directory' afterwards.")

(defvar user-dremacs-directory
  (file-name-as-directory
   (expand-file-name
    (or (getenv "DREMACSDIR")
        (file-name-concat (file-name-directory (directory-file-name dremacs-directory))
                          ".dremacs.d"))))
  "Root directory for the user's DrEmacs configuration.
Defaults to a `.dremacs.d' directory next to `dremacs-directory';
override it with the DREMACSDIR environment variable.")

(unless (file-exists-p user-dremacs-directory)
  ;; COPY-CONTENTS and PARENTS: copy the template's contents into
  ;; `user-dremacs-directory' itself, not into a subdirectory of it.
  (copy-directory (file-name-concat dremacs-directory ".template" ".dremacs.d")
                  user-dremacs-directory nil t t))

(let ((early-init-path (locate-file "early-init" (list user-dremacs-directory) load-suffixes)))
  (when early-init-path
    (load early-init-path t t t)))
