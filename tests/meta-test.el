;;; meta-test.el --- Tests for the Meta module system  -*- lexical-binding: t -*-

;; Run from the repository root:
;;
;;   make test
;;
;; or directly:
;;
;;   emacs -Q --batch -l tests/meta-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(load (expand-file-name "../pkgs/meta/main.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar meta-test-log nil
  "Test modules push a symbol here each time their top level runs.")

(defvar meta-test-counter 0
  "Counter bumped by `eval-after-load' callbacks in tests.")

(defmacro meta-test-with-sandbox (&rest body)
  "Run BODY with empty Meta registries and a fresh directory bound to `root'."
  (declare (indent 0))
  `(let ((meta-installed-scopes (make-hash-table :test 'equal))
         (meta-installed-packages (make-hash-table :test 'equal))
         (meta-installed-collections (make-hash-table :test 'equal))
         (meta-modules (make-hash-table :test 'equal))
         (meta--loading nil)
         (meta-test-log nil)
         (root (file-name-as-directory (make-temp-file "meta-test-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun meta-test-write (root file content)
  "Write CONTENT to FILE under ROOT, creating directories as needed."
  (let ((path (expand-file-name file root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defun meta-test-package (root scope package collection)
  "Create PACKAGE in SCOPE under ROOT, providing COLLECTION."
  (meta-test-write root (format "%s/%s/metadata.el" scope package)
                   (format "(:collection %S)" collection)))

(defun meta-test-module (root scope package file tag)
  "Create module FILE in SCOPE/PACKAGE under ROOT that logs TAG when run."
  (meta-test-write root (format "%s/%s/%s" scope package file)
                   (format "(push '%s meta-test-log)\n" tag)))

(defun meta-test-install (root scope)
  (meta-install-scope scope (expand-file-name scope root)))

;;;; Resolution

(ert-deftest meta-test-higher-root-wins-across-file-forms ()
  "A higher-priority root wins even if a lower one has the `X.el' form."
  (meta-test-with-sandbox
    (meta-test-package root "sys" "a" "mt-shadow")
    (meta-test-module root "sys" "a" "m.el" 'system)
    (meta-test-package root "usr" "b" "mt-shadow")
    (meta-test-module root "usr" "b" "m/main.el" 'user)
    (meta-test-install root "sys")
    (meta-test-install root "usr")
    (meta-dynamic-import '(mt-shadow m))
    (should (equal meta-test-log '(user)))))

(ert-deftest meta-test-main-fallback ()
  "(C) names main.el; (C X) falls back to X/main.el; (C X main) is the same module."
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-main")
    (meta-test-module root "s" "p" "main.el" 'top)
    (meta-test-module root "s" "p" "x/main.el" 'x)
    (meta-test-install root "s")
    (meta-dynamic-import '(mt-main))
    (meta-dynamic-import '(mt-main x))
    (meta-dynamic-import '(mt-main x main))
    (should (equal meta-test-log '(x top)))
    (should (featurep 'meta:mt-main/x))))

(ert-deftest meta-test-scope-reinstall-is-idempotent ()
  (meta-test-with-sandbox
    (meta-test-package root "sys" "a" "mt-reinstall")
    (meta-test-package root "usr" "b" "mt-reinstall")
    (meta-test-install root "sys")
    (meta-test-install root "usr")
    (meta-test-install root "sys")
    (meta-test-install root "usr")
    (let ((roots (gethash "mt-reinstall" meta-installed-collections)))
      (should (= (length roots) 2))
      (should (string-match-p "/usr/b\\'" (car roots))))))

(ert-deftest meta-test-unknown-library ()
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-unknown")
    (meta-test-install root "s")
    (should-error (meta-dynamic-import '(mt-unknown nope)))
    (should-error (meta-dynamic-import '(mt-no-such-collection)))))

;;;; Loading

(ert-deftest meta-test-loads-once ()
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-once")
    (meta-test-module root "s" "p" "m.el" 'm)
    (meta-test-install root "s")
    (meta-dynamic-import '(mt-once m))
    (meta-dynamic-import '(mt-once m))
    (should (equal meta-test-log '(m)))))

(ert-deftest meta-test-retry-after-failure ()
  "A module that fails is not recorded, so it can be imported again once fixed."
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-retry")
    (meta-test-write root "s/p/m.el" "(push 'broken meta-test-log) (error \"boom\")")
    (meta-test-install root "s")
    (should-error (meta-dynamic-import '(mt-retry m)))
    (should-not (featurep 'meta:mt-retry/m))
    (meta-test-module root "s" "p" "m.el" 'fixed)
    (meta-dynamic-import '(mt-retry m))
    (should (equal meta-test-log '(fixed broken)))
    (should (featurep 'meta:mt-retry/m))))

(ert-deftest meta-test-cycles-are-reported ()
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-cycle")
    (meta-test-write root "s/p/a.el" "(meta-dynamic-import '(mt-cycle b))")
    (meta-test-write root "s/p/b.el" "(meta-dynamic-import '(mt-cycle a))")
    (meta-test-write root "s/p/self.el" "(meta-dynamic-import '(mt-cycle self))")
    (meta-test-install root "s")
    (let ((err (should-error (meta-dynamic-import '(mt-cycle a)))))
      (should (string-match-p "Cycle in loading: .*/a -> .*/b -> .*/a\\'"
                              (error-message-string err))))
    (should (= (hash-table-count meta-modules) 0))
    (should-error (meta-dynamic-import '(mt-cycle self)))))

;;;; Features

(ert-deftest meta-test-features-are-namespaced ()
  "Importing a collection never provides a bare feature of the same name."
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-bare")
    (meta-test-module root "s" "p" "main.el" 'main)
    (meta-test-install root "s")
    (meta-dynamic-import '(mt-bare))
    (should-not (featurep 'mt-bare))
    (should (featurep 'meta:mt-bare))))

(ert-deftest meta-test-foreign-provide-does-not-skip-module ()
  "Loading is decided by `meta-modules', not by `featurep'."
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-foreign")
    (meta-test-module root "s" "p" "m.el" 'm)
    (meta-test-install root "s")
    (provide 'mt-foreign/m)
    (provide 'meta:mt-foreign/m)
    (meta-dynamic-import '(mt-foreign m))
    (should (equal meta-test-log '(m)))))

(ert-deftest meta-test-after-load-callbacks-run-once ()
  "Repeated imports must not rerun `eval-after-load' callbacks."
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-notify")
    (meta-test-module root "s" "p" "main.el" 'main)
    (meta-test-install root "s")
    (setq meta-test-counter 0)
    (let ((load-file-name nil))         ; as from an interactive call
      (with-eval-after-load 'meta:mt-notify (setq meta-test-counter (1+ meta-test-counter)))
      (meta-dynamic-import '(mt-notify))
      (meta-dynamic-import '(mt-notify))
      (meta-dynamic-import '(mt-notify)))
    (should (= meta-test-counter 1))))

;;;; Autoload stubs

(ert-deftest meta-test-auto-import-loads-once ()
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-auto")
    (meta-test-write root "s/p/m.el"
                     "(push 'm meta-test-log) (defun meta-test-auto-fn (x) (* 2 x))")
    (meta-test-install root "s")
    (meta-dynamic-auto-import 'meta-test-auto-fn '(mt-auto m))
    (should (null meta-test-log))
    (should (= (meta-test-auto-fn 21) 42))
    (meta-dynamic-import '(mt-auto m))
    (should (equal meta-test-log '(m)))))

(ert-deftest meta-test-auto-import-command ()
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-cmd")
    (meta-test-write root "s/p/m.el"
                     "(defun meta-test-auto-cmd (n) (interactive \"p\") (* 10 n))")
    (meta-test-install root "s")
    (meta-dynamic-auto-import 'meta-test-auto-cmd '(mt-cmd m) nil t)
    (should (commandp 'meta-test-auto-cmd))
    (should (= (let ((current-prefix-arg 3))
                 (call-interactively 'meta-test-auto-cmd))
               30))))

(ert-deftest meta-test-auto-import-retries-after-partial-load ()
  "A module that defines the function and then fails must not leave the
half-loaded definition in place of the stub."
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-partial")
    (meta-test-write root "s/p/m.el"
                     "(defun meta-test-partial-fn () 'partial) (error \"init failed\")")
    (meta-test-install root "s")
    (meta-dynamic-auto-import 'meta-test-partial-fn '(mt-partial m))
    (should-error (meta-test-partial-fn))
    (meta-test-write root "s/p/m.el" "(defun meta-test-partial-fn () 'fixed)")
    (should (eq (meta-test-partial-fn) 'fixed))))

(ert-deftest meta-test-auto-import-docstring ()
  "The stub's docstring describes the stub only."
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-doc")
    (meta-test-write root "s/p/m.el" "(defun meta-test-doc-fn () \"Real doc.\" nil)")
    (meta-test-install root "s")
    (meta-dynamic-auto-import 'meta-test-doc-fn '(mt-doc m) "Stub doc.")
    (should (equal (documentation 'meta-test-doc-fn) "Stub doc."))
    (meta-test-doc-fn)
    (should (equal (documentation 'meta-test-doc-fn) "Real doc."))))

(ert-deftest meta-test-auto-import-errors ()
  (meta-test-with-sandbox
    (meta-test-package root "s" "p" "mt-auto-err")
    (meta-test-write root "s/p/m.el" "nil")
    (meta-test-install root "s")
    (should-error (meta-dynamic-auto-import 'meta-test-never '(mt-auto-err nope)))
    (should-not (fboundp 'meta-test-never))
    (meta-dynamic-auto-import 'meta-test-undefined-fn '(mt-auto-err m))
    (should-error (meta-test-undefined-fn))))

;;;; Metadata

(ert-deftest meta-test-metadata ()
  (meta-test-with-sandbox
    ;; "" makes every subdirectory a collection.
    (meta-test-write root "s/multi/metadata.el" "(:collection \"\")")
    (make-directory (expand-file-name "s/multi/mt-one" root) t)
    (make-directory (expand-file-name "s/multi/mt-two" root) t)
    (meta-test-install root "s")
    (should (gethash "mt-one" meta-installed-collections))
    (should (gethash "mt-two" meta-installed-collections))
    ;; Metadata is data: the old evaluated `definfo' form is rejected.
    (meta-test-write root "old/p/metadata.el" "(definfo x (list :collection \"x\") \"d\")")
    (should-error (meta-test-install root "old"))
    ;; Missing dependencies are reported.
    (meta-test-write root "deps/p/metadata.el" "(:collection \"x\" :deps (\"nope\"))")
    (should-error (meta-test-install root "deps"))))

;;; meta-test.el ends here
