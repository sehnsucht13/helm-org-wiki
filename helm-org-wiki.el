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

(defcustom helm-org-wiki-reading-list-heading "Reading List"
  "Variable which stores the name of the heading under which all org-links which correspond to a reading list item are stored."
  :group 'helm-org-wiki
  :type 'string)

(defcustom helm-org-wiki-source-block-options ":results output code"
  "Contains the options that are put into the header of a new source block."
  :group 'helm-org-wiki
  :type 'string)


(defcustom helm-org-wiki-new-book-template '("ADD-BOOK" "Books" plain (file+headline helm-org-wiki-index "Reading List")
                 "\n%(helm-org-wiki--get-org-link)")
"Contains the \"org-capture\" template used to add a book to the helm-org-wiki reading list."
  :group 'helm-org-wiki
  :type 'list)

;; General Wiki management functions
(defun helm-org-wiki-create-new-wiki (WIKI-PATH)
  "Create a new wiki directory along with an index in the location specified by WIKI-PATH."
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
  "Open a Helm buffer at the wiki root and walk through the wiki to the file you want to open."
  (interactive)
  (helm-find-files-1 helm-org-wiki-directory)
  (with-current-buffer
	  (if (not (= 0 (buffer-size)))
			   (helm-org-in-buffer-headings))))


(defun helm-org-wiki-create-new-article (NEW-ARTICLE-NAME)
  "Create a new article in the same directory which is named NEW-ARTICLE-NAME.  Requires that an article in the wiki is currently visited."
  (interactive "sEnter article name: ")
  (find-file (concat (file-name-directory (buffer-file-name))  NEW-ARTICLE-NAME))
  (save-buffer))


(defun helm-org-wiki-rename-entry (NEW-NAME &rest save-on-rename)
  "Rename the current Wiki entry to NEW-NAME.  If SAVE-ON-RENAME is true then the buffer is saved as well."
  (interactive "sEnter a new file name ")
  (rename-file (buffer-file-name) NEW-NAME)
  (rename-buffer NEW-NAME)
  (set-visited-file-name NEW-NAME)
  (if save-on-rename
	  (save-current-buffer)))

(defun helm-org-wiki-extract-subtree (NEW-NAME)
  "Extract the subtree at point into a new file that is within the same subdirectory as the current visited wiki article.  File is saved as NEW-NAME."
  (interactive "sEnter article name: ")
  (let ((path (file-name-directory (buffer-file-name))))
	(org-cut-subtree)
	(with-temp-buffer
	  (org-yank)
	  (write-file (concat path NEW-NAME)))))

;; Reading list functions below

;;This function is kind of a hack and I hope there might be a better way to implement this in the future.
;; The current wiki index is opened up in a temp buffer, then a search forward is done for the reading list heading
;; and as soon as this is located, the subtree and everything in it is copied and stored in the killring.
;; From there, another temp buffer is made and the subtree is yanked there. After this, a lambda function is mapped
;; on each element of the subtree and the link descriptions are extracted and returned as a list.
(defun helm-org-wiki-retrieve-reading-list ()
  "Retrieve the reading list from the wiki index."
  (interactive)
  (with-temp-buffer
	(find-file helm-org-wiki-index)                   ;; Open the wiki index
	(search-forward helm-org-wiki-reading-list-heading)
	(org-copy-subtree)
	(kill-current-buffer))
	(with-temp-buffer
	  (org-yank)
	  (let ((READING-LIST-NAMES nil))
		(org-element-map (org-element-parse-buffer) 'link
		  (lambda (links)
			(push (list (nth 2 links)) READING-LIST-NAMES)))
		READING-LIST-NAMES)))

;; This function opens up the selected book. It simply takes the name(that is the description of the org-mode link made by the user) and searches forward for it and opens it. The BIG drawback is that the names of the links have to be unique otherwise the book might not be opened reliably.
(defun helm-org-wiki-open-book (candidate)
  "This function opens the book selected by the user through the Helm menu provided.  CANDIDATE represents the book name that is to be opened."
  (with-temp-buffer
	(find-file helm-org-wiki-index)
	(search-forward candidate)
	(org-open-at-point)
	(kill-current-buffer)))

(defun helm-org-wiki-remove-book (candidate)
	"Remove the selected book indicated by CANDIDATE from the reading list."
  (with-temp-buffer
	(find-file helm-org-wiki-index)
	(search-forward candidate)
	(org-kill-line)
	(kill-current-buffer)))

(defun helm-org-wiki--get-org-link ()
  "Small helper function that grabs an \"org-mode\" link and return it as string."
  (with-temp-buffer
	(org-insert-link-global)
	(beginning-of-line)
	(kill-line))
  (let ((kill-ring-pop-value (pop kill-ring)))
	kill-ring-pop-value))

(defun helm-org-wiki-add-book-to-reading-list ()
  "Add a new book(or any file) to the reading list."
  (interactive)
  (push helm-org-wiki-new-book-template org-capture-templates)
  (org-capture nil "ADD-BOOK")
  (org-capture-finalize)
  (pop org-capture-templates))

(defun helm-org-wiki-open-reading-list ()
  "Open the reading list."
  (interactive)
  (helm :sources (helm-build-sync-source "Reading List"
				   :candidates (helm-org-wiki-retrieve-reading-list)
				   :action
				   (helm-make-actions
					"Open Book"
					'helm-org-wiki-open-book
					"Remove Book From List"
					'helm-org-wiki-remove-book))
		:buffer "*Reading List Buffer*"))

;;; Code blocks below
(defun helm-org-wiki-emacs-lisp-block ()
  "Insert an Emacs-lisp block."
  (interactive)
  (insert (concat "#+BEGIN_SRC emacs-lisp " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-python-block ()
  "Insert a Python code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC python " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-latex-block ()
  "Insert a Latex code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC latex " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-java-block ()
  "Insert a Java code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC java " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-javascript-block ()
  "Insert a Javascript code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC js " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-sh-block ()
  "Insert a Shell script code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC sh " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-haskell-block ()
  "Insert a Haskell code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC haskell " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-C-block ()
  "Insert a C code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC C " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-C++-block ()
  "Insert a C++ code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC C++ " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-awk-block ()
  "Insert a Awk code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC awk " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-clojure-block ()
  "Insert a Clojure code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC clojure " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-R-block ()
  "Insert an R code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC R " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-perl-block ()
  "Insert a Perl code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC perl " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-Gnuplot-block ()
  "Insert a Gnuplot code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC gnuplot " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-D-block ()
  "Insert a D code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC D " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-ruby-block ()
  "Insert a Ruby code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC ruby " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-sed-block ()
  "Insert a Sed code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC sed " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(defun helm-org-wiki-CSS-block ()
  "Insert a CSS code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC css " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))
(defun helm-org-wiki-lisp-block ()
  "Insert a Common Lisp code block."
  (interactive)
  (insert (concat "#+BEGIN_SRC lisp " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

;;; Anything below here requires a plugin to be evaluated
(defun helm-org-wiki-rust-block ()
  "Insert a Rust code block.  Requires the ob-rust plugin available from MELPA."
  (interactive)
  (insert (concat "#+BEGIN_SRC rust " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-go-block ()
  "Insert a Golang code block.  Requires the ob-go plugin available from MELPA."
  (interactive)
  (insert (concat "#+BEGIN_SRC go " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))


(defun helm-org-wiki-typescript-block ()
  "Insert a Typescript code block.  Requires the ob-typescript plugin available from MELPA."
  (interactive)
  (insert (concat "#+BEGIN_SRC typescript " helm-org-wiki-source-block-options))
  (org-return)
  (org-return)
  (insert "#+END_SRC")
  (previous-line 1))

(provide 'helm-org-wiki)
;;; helm-org-wiki.el ends here
