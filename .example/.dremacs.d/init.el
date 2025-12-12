;;; init.el --- User initialization file -*- lexical-binding: t -*-

(let ((scope-path (file-name-concat user-dremacs-directory "pkgs")))
  (meta-install-scope "user" scope-path))

(meta-import (private))
