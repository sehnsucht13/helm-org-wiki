;;; package --- Summary
;;; Commentary:
(require 'org)
(require 'helm)

;;; Code:
(defcustom helm-org-wiki-directory "~/Wiki"
  "This variable contains the path to the main folder which houses the wiki."
  :group 'helm-org-wiki
  :type 'string)

(defcustom helm-org-wiki-index "~/Wiki/index.org"
  "Variable to hold the path to the wiki index."
  :group 'helm-org-wiki
  :type 'string)

(defun helm-org-wiki-open-index ()
  "Opens the wiki index."
  (interactive)
  (find-file helm-org-wiki-index))

(defun helm-org-wiki--make-helm-src ()
  "Create the sources for the wiki index."
  (let ((DIR-LIST (directory-files helm-org-wiki-directory)))
	(pop DIR-LIST)
	(pop DIR-LIST)
	DIR-LIST
	))

(defun wiki-create-new-section ()
  "Create a new section into the wiki."
  (interactive)
  (helm :sources
		(helm-build-sync-source "*New Wiki Section Source*"
		  :candidates (helm-org-wiki--make-helm-src))
		:buffer "*New Wiki Section*"))


(defun helm-org-wiki-emacs-lisp-block ()
  "Insert an Emacs-lisp block."
  (interactive)
  (insert "#+BEGIN_SRC emacs-lisp")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-python-block ()
  "Insert a Python code block."
  (interactive)
  (insert "#+BEGIN_SRC python")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-latex-block ()
  "Insert a Latex code block."
  (interactive)
  (insert "#+BEGIN_SRC latex")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-java-block ()
  "Insert a Java code block."
  (interactive)
  (insert "#+BEGIN_SRC java")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-javascript-block ()
  "Insert a Javascript code block."
  (interactive)
  (insert "#+BEGIN_SRC js")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-sh-block ()
  "Insert a Shell script code block."
  (interactive)
  (insert "#+BEGIN_SRC sh")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-haskell-block ()
  "Insert a Haskell code block."
  (interactive)
  (insert "#+BEGIN_SRC haskell")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-C-block ()
  "Insert a C code block."
  (interactive)
  (insert "#+BEGIN_SRC C")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-C++-block ()
  "Insert a C++ code block."
  (interactive)
  (insert "#+BEGIN_SRC C++")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-lisp-block ()
  "Insert a Common Lisp code block."
  (interactive)
  (insert "#+BEGIN_SRC lisp")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

;;; Anything below here requires a plugin to be executed
(defun helm-org-wiki-rust-block ()
  "Insert a Rust code block. Requires the ob-rust plugin available from MELPA."
  (interactive)
  (insert "#+BEGIN_SRC rust")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-go-block ()
  "Insert a Golang code block. Requires the ob-go plugin available from MELPA."
  (interactive)
  (insert "#+BEGIN_SRC go")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-typescript-block ()
  "Insert a Typescript code block. Requires the ob-typescript plugin available from MELPA."
  (interactive)
  (insert "#+BEGIN_SRC typescript")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(provide 'helm-org-wiki)
;;; helm-org-wiki.el ends here
