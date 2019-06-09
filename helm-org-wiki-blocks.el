;;; package --- Summary
;;;; License:
;; Commentary:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defcustom helm-org-wiki-source-block-options ":results output raw"
  "Contains the options that are put into the header of a new source block."
  :group 'helm-org-wiki
  :type 'string)

(defcustom helm-org-wiki-edit-source-block-on-creation t
  "Instantly enter special edit mode for the newly created source block."
  :group 'helm-org-wiki
  :type 'string)

(defcustom helm-org-wiki-load-source-blocks t
  "Enable the loading of source block functions."
  :group 'helm-org-wiki
  :type 'boolean)

(if helm-org-wiki-load-source-blocks
  (progn
	(defun helm-org-wiki-emacs-lisp-block ()
	"Insert an Emacs-lisp source block."
	(interactive)
	(insert (concat "#+BEGIN_SRC emacs-lisp " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))

	(defun helm-org-wiki-python-block ()
	"Insert a Python source block."
	(interactive)
	(insert (concat "#+BEGIN_SRC python " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))


	(defun helm-org-wiki-latex-block ()
	"Insert a Latex code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC latex " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))


	(defun helm-org-wiki-java-block ()
	"Insert a Java code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC java " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))


	(defun helm-org-wiki-javascript-block ()
	"Insert a Javascript code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC js " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))


	(defun helm-org-wiki-sh-block ()
	"Insert a Shell script code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC sh " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))


	(defun helm-org-wiki-haskell-block ()
	"Insert a Haskell code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC haskell " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))

	(defun helm-org-wiki-C-block ()
	"Insert a C code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC C " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))


	(defun helm-org-wiki-C++-block ()
	"Insert a C++ code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC C++ " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))

	(defun helm-org-wiki-awk-block ()
	"Insert a Awk code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC awk " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
		(org-edit-src-code)))


	(defun helm-org-wiki-clojure-block ()
	"Insert a Clojure code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC clojure " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))


	(defun helm-org-wiki-R-block ()
	"Insert an R code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC R " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	(defun helm-org-wiki-perl-block ()
	"Insert a Perl code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC perl " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	(defun helm-org-wiki-Gnuplot-block ()
	"Insert a Gnuplot code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC gnuplot " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	(defun helm-org-wiki-D-block ()
	"Insert a D code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC D " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	(defun helm-org-wiki-ruby-block ()
	"Insert a Ruby code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC ruby " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	(defun helm-org-wiki-sed-block ()
	"Insert a Sed code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC sed " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	(defun helm-org-wiki-CSS-block ()
	"Insert a CSS code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC css " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	(defun helm-org-wiki-lisp-block ()
	"Insert a Common Lisp code block."
	(interactive)
	(insert (concat "#+BEGIN_SRC lisp " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))

	;;; Anything below here requires a plugin to be evaluated
	(defun helm-org-wiki-rust-block ()
	"Insert a Rust code block.  Requires the ob-rust plugin available from MELPA for evaluation."
	(interactive)
	(insert (concat "#+BEGIN_SRC rust " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))


	(defun helm-org-wiki-go-block ()
	"Insert a Golang code block.  Requires the ob-go plugin available from MELPA for evaluation."
	(interactive)
	(insert (concat "#+BEGIN_SRC go " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))


	(defun helm-org-wiki-typescript-block ()
	"Insert a Typescript code block.  Requires the ob-typescript plugin available from MELPA for evaluation."
	(interactive)
	(insert (concat "#+BEGIN_SRC typescript " helm-org-wiki-source-block-options))
	(org-return)
	(org-return)
	(insert "#+END_SRC")
	(previous-line 1)
	(if helm-org-wiki-edit-source-block-on-creation
				(org-edit-src-code)))))


(provide 'helm-org-wiki-blocks)
;;; helm-org-wiki-blocks.el ends here
