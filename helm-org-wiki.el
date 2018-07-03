;;; package --- Summary
;;; Commentary:
(require 'org)
(require 'helm)

;;; Code:
(defun open-wiki-index ()
"Opens the wiki index."
(interactive)
(find-file "~/Wiki/index.org"))

(defun wiki-emacs-lisp-block ()
  "Insert an Emacs-lisp block."
(interactive)
(insert "#+BEGIN_SRC emacs-lisp")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))

(defun wiki-python-block ()
  "Insert a Python code block."
(interactive)
(insert "#+BEGIN_SRC python")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))

(defun wiki-latex-block ()
  "Insert a Latex code block."
(interactive)
(insert "#+BEGIN_SRC latex")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))

(defun wiki-java-block ()
  "Insert a Java code block."
(interactive)
(insert "#+BEGIN_SRC java")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))


(defun wiki-javascript-block ()
  "Insert a Javascript code block."
(interactive)
(insert "#+BEGIN_SRC js")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))

(defun wiki-sh-block ()
  "Insert a Shell script code block."
(interactive)
(insert "#+BEGIN_SRC sh")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))


(defun wiki-haskell-block ()
  "Insert a Haskell code block."
(interactive)
(insert "#+BEGIN_SRC haskell")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))


(defun wiki-C-block ()
  "Insert a C code block."
(interactive)
(insert "#+BEGIN_SRC C")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))


(defun wiki-C++-block ()
  "Insert a C++ code block."
(interactive)
(insert "#+BEGIN_SRC C++")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))


(defun wiki-rust-block ()
  "Insert a Rust code block."
(interactive)
(insert "#+BEGIN_SRC rust")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))

(defun wiki-lisp-block ()
  "Insert a Common Lisp code block."
(interactive)
(insert "#+BEGIN_SRC lisp")
(org-return)
(org-return)
(insert "#+END_SRC")
(previous-line 1))

(provide 'helm-org-wiki)
;;; helm-org-wiki.el ends here
