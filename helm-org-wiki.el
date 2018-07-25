;;; package --- Summary
;;; Commentary:
(require 'org)
(require 'helm)

;;; Code:
(defcustom helm-org-wiki-directory "~/Wiki/"
  "This variable contains the path to the main folder which houses the wiki."
  :group 'helm-org-wiki
  :type 'string)

(defcustom helm-org-wiki-index "~/Wiki/Index.org"
  "Variable to hold the path to the wiki index."
  :group 'helm-org-wiki
  :type 'string)

(defun helm-org-wiki-create-new-wiki (WIKI-PATH)
  "Create a new wiki directory along with an index."
  (interactive "DEnter path to wiki: ")
  (if (not (file-directory-p WIKI-PATH)) ;;Prevent overwriting a directory
	  (progn
		(make-directory WIKI-PATH)
		(with-temp-buffer
		  (insert "* Wiki Index")
		  (write-file (concat WIKI-PATH "/Index.org"))))
	(message "This directory already exists")))

(defun helm-org-wiki-open-index ()
  "Open the wiki index."
  (interactive)
  (find-file helm-org-wiki-index))

(defun helm-org-wiki-walk-wiki ()
  "Wrapper function for conveniance."
  (interactive)
  (helm-find-files-1 helm-org-wiki-directory))

(defun helm-org-wiki-create-new-article (newArticleName)
  "Create a new article in the same directory.  Requires that an article in the wiki is currently visited."
  (interactive "sEnter article name: ")
  (find-file (concat (file-name-directory (buffer-file-name))  newArticleName))
  (save-buffer))

(defun helm-org-wiki-rename-entry (NEW-NAME &rest save-on-rename)
  "Rename the current Wiki entry to NEW-NAME.  If SAVE-ON-RENAME is true then the buffer is saved as well."
  (interactive "sEnter a new file name ")
  (rename-file (buffer-file-name) NEW-NAME)
  (rename-buffer NEW-NAME)
  (set-visited-file-name NEW-NAME)
  (if save-on-rename
	  (save-current-buffer)))

;;; Code blocks below
(defun helm-org-wiki-emacs-lisp-block ()
  "Insert an Emacs-lisp block."
  (interactive)
  (insert "#+BEGIN_SRC emacs-lisp :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-python-block ()
  "Insert a Python code block."
  (interactive)
  (insert "#+BEGIN_SRC python :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-latex-block ()
  "Insert a Latex code block."
  (interactive)
  (insert "#+BEGIN_SRC latex :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-java-block ()
  "Insert a Java code block."
  (interactive)
  (insert "#+BEGIN_SRC java :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-javascript-block ()
  "Insert a Javascript code block."
  (interactive)
  (insert "#+BEGIN_SRC js :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-sh-block ()
  "Insert a Shell script code block."
  (interactive)
  (insert "#+BEGIN_SRC sh :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-haskell-block ()
  "Insert a Haskell code block."
  (interactive)
  (insert "#+BEGIN_SRC haskell :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-C-block ()
  "Insert a C code block."
  (interactive)
  (insert "#+BEGIN_SRC C :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-C++-block ()
  "Insert a C++ code block."
  (interactive)
  (insert "#+BEGIN_SRC C++ :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-lisp-block ()
  "Insert a Common Lisp code block."
  (interactive)
  (insert "#+BEGIN_SRC lisp :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

;;; Anything below here requires a plugin to be evaluated
(defun helm-org-wiki-rust-block ()
  "Insert a Rust code block.  Requires the ob-rust plugin available from MELPA."
  (interactive)
  (insert "#+BEGIN_SRC rust :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-go-block ()
  "Insert a Golang code block.  Requires the ob-go plugin available from MELPA."
  (interactive)
  (insert "#+BEGIN_SRC go :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-typescript-block ()
  "Insert a Typescript code block.  Requires the ob-typescript plugin available from MELPA."
  (interactive)
  (insert "#+BEGIN_SRC typescript :results output code")
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(provide 'helm-org-wiki)
;;; helm-org-wiki.el ends here
