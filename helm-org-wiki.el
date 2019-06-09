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

(require 'org)
(require 'helm)
(require 'helm-org-wiki-blocks)

;;; Code:
(defcustom helm-org-wiki-directories '(("Personal Wiki" . "~/Wiki/"))
  "This variable contains the names and paths to the main folders which houses each wiki."
  :group 'helm-org-wiki
  :type 'alist)

(defcustom helm-org-wiki-reading-list-heading "Reading List"
  "Variable which stores the name of the heading under which all org-links which correspond to a reading list item are stored."
  :group 'helm-org-wiki
  :type 'string)

(defcustom helm-org-wiki-new-book-template '("ADD-BOOK" "Books" plain (file+headline helm-org-wiki-index "Reading List")
                 "\n%(helm-org-wiki--get-org-link)")
"Contains the \"org-capture\" template used to add a book to the helm-org-wiki reading list."
  :group 'helm-org-wiki
  :type 'list)


(defcustom helm-org-wiki-screenshot-wait-time 3
  "Seconds to wait until the screenshot application is called."
  :group 'helm-org-wiki
  :type 'number)

;; General Wiki management functions
(defun helm-org-wiki--helm-or-action (singleAction multipleAction)
  "Execute SINGLEACTION if there is only one wiki or MULTIPLEACTION if there are two or more wiki directiories."
  (interactive)
  (let ((headingList (mapcar 'car helm-org-wiki-directories)))
	(if (= 1 (length headingList))
		(funcall singleAction headingList)
	  (funcall multipleAction headingList))))

(defun helm-org-wiki-create-new-wiki (WIKI-PATH)
  "Create a new wiki directory along with an index in the location specified by WIKI-PATH."
  (interactive "DEnter path to wiki: ")
  (if (not (file-directory-p WIKI-PATH)) ;;Prevent overwriting a directory
	  (progn
		(make-directory WIKI-PATH)
		(with-temp-buffer
		  (insert "* Wiki Index")
		  (newline)
		  (insert (concat "* " helm-org-wiki-reading-list-heading))
		  (write-file (concat WIKI-PATH "/Index.org"))))
	(message "This directory already exists")))

(defclass helm-org-wiki-find-file-class (helm-source-ffiles)
  ())

(defun helm-org-wiki-open-index ()
  "Open the wiki index."
  (interactive)
  (find-file helm-org-wiki-index))

(defun helm-org-wiki--visit-root (wikiName)
  "Visit the wiki associated with the name WIKINAME."
  (helm-find-files-1 (cdr(assoc-string wikiName helm-org-wiki-directories))))

(defun helm-org-wiki--delete-wiki (wikiName)
  "Delete the wiki associated with the name WIKINAME."
  (delete-directory (cdr (assoc-string wikiName helm-org-wiki-directories))))
  
(defun helm-org-wiki-visit-single-wiki (candidates)
  "Visit the wiki with path CANDIDATES when there is a only a single root."
(helm-find-files-1 (alist-get (car candidates) helm-org-wiki-directories)))

(defun make-wiki-action-buffer (candidates)
  "Create a helm buffer containing CANDIDATES when there is more than one wiki root available."
  (helm :sources (helm-build-sync-source "Wiki List"
				   :candidates candidates
				   :action
				   (helm-make-actions
					"Visit Wiki"
					'helm-org-wiki--visit-root
					"Delete Wiki"
					'helm-org-wiki--delete-wiki
					))
		:buffer "*Wiki Root Buffer*"))
  
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
	(outline-show-all)
	(search-forward candidate)
	(kill-whole-line)
	(save-buffer)
	(kill-current-buffer)))

(defun helm-org-wiki-rename-book (candidate)
	"Edit the link named CANDIDATE to change its description or path."
  (with-temp-buffer
	(find-file helm-org-wiki-index)
	(search-forward candidate)
	(org-insert-link)
	(save-buffer)
	(kill-current-buffer)))

(defun helm-org-wiki--get-org-link ()
  "Small helper function that grabs an \"org-mode\" link and return it as string."
  (with-temp-buffer
	(org-insert-link-global)
	(beginning-of-line)
	(kill-line))
  (let ((kill-ring-pop-value (pop kill-ring)))
	kill-ring-pop-value))

(defun helm-org-wiki-add-book-to-reading-list (&rest candidate)
  "Add a new book(or any file) to the reading list."
  (interactive)
  (push helm-org-wiki-new-book-template org-capture-templates)
  (org-capture nil "ADD-BOOK")
  (org-capture-finalize)
  (pop org-capture-templates))

(defun helm-org-wiki-open-reading-list ()
  "Open a helm buffer which displays the reading list."
  (interactive)
  (helm :sources (helm-build-sync-source "Reading List"
				   :candidates (helm-org-wiki-retrieve-reading-list)
				   :action
				   (helm-make-actions
					"Open Book"
					'helm-org-wiki-open-book
					"Edit Book Link"
					'helm-org-wiki-rename-book
					"Remove Book From List"
					'helm-org-wiki-remove-book
					"Add a Book To List"
					'helm-org-wiki-add-book-to-reading-list))
		:resume nil
		:buffer "*Reading List Buffer*"))

(defun start-screenshotter()
  (interactive) 
  (sleep-for helm-org-wiki-screenshot-wait-time)
  (start-process "helm-org-wiki-screenshot-process" nil "xfce4-screenshooter" "-r"))

(provide 'helm-org-wiki)
;;; helm-org-wiki.el ends here
