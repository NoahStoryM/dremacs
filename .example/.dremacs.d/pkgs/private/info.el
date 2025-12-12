;;; meta.el --- Private package metadata -*- lexical-binding: t -*-

(definfo private-pkg-info
  (list :collection "private"
        :pkg-desc "User private package"
        :deps '("meta"))
  "Metadata for the `private' collection system.")
(meta-export (private info))
