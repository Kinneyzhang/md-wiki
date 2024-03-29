;;; md-wiki.el --- wiki generator to static site on top of markdown.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/md-wiki
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Md-wiki is wiki generator to static site on top of markdown.

;;; Code:

(defvar md-wiki-dir "~/geekblog/content/gknows"
  "Directory of all md-wiki pages.")

(defvar md-wiki-tree-file "~/mdwiki.org"
  "The org file showing the tree structure of mdwiki pages. 
Feel free to modify it to add, update or delete pages.")

(defvar md-wiki-diff-file (concat user-emacs-directory "mdwiki-diff.el")
  "The structure used to diff")

(defvar md-wiki-index-page "index")

(defvar md-wiki-index-page-shown "INDEX")

(defvar md-wiki-prefix-url "/gknows")

(defvar md-wiki-parents-html-wrapper "<span><center>%s</center></span>")

(defvar md-wiki-children-html-wrapper "<small><center>%s</center></small>")

(defvar md-wiki-parents-nav-prefix "")

(defvar md-wiki-parents-nav-sep " > ")

(defvar md-wiki-children-nav-prefix "")

(defvar md-wiki-children-nav-sep " | ")

(defvar md-wiki-meta-fmt "---\ntitle: %s\ntype: post\nlayout: daily\ntoc: false\ncomment: false\n---")

(defvar md-wiki-browse-url-base "https://geekinney.com/gknows/")

(defvar md-wiki-bookmark-list '("Daily Page" "费曼学习法" "Emacs"))

(defvar md-wiki-bookmark-separator " / ")

(defvar md-wiki-bookmark-prefix "快捷访问> ")


;;;; Utilities
(defun file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defmacro with-file-buffer (file &rest body)
  (declare (indent defun))
  `(with-current-buffer (find-file-noselect ,file)
     (save-excursion
       (goto-char (point-min))
       ,@body)))

(defun md-wiki-page-file (page)
  (expand-file-name (concat page ".md") md-wiki-dir))

(defun md-wiki-page-p ()
  "judge if current buffer belongs to a md-wiki page."
  (file-equal-p (file-name-directory (buffer-file-name)) md-wiki-dir))

;; (defun md-wiki-page-exist (title)
;;   "Judge if it exists the page with TITLE."
;;   )

(defun md-wiki-curr-page-title ()
  "return the title of current buffer page."
  (if (md-wiki-page-p)
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (if (re-search-forward "---\n\\(.+\n\\)+?---" nil t)
              (let ((header (match-string-no-properties 0)))
                (string-match "title: *\\(.+\\)" header)
                (string-trim (match-string-no-properties 1 header)))
            (error "Wrong header format of current page!"))))
    (error "Current is not a md-wiki page!")))

(defun md-wiki-structures ()
  (unless (file-exists-p md-wiki-tree-file)
    (error "Md-wiki tree file not exists: please refer to `md-wiki-tree-file'!"))
  (with-file-buffer md-wiki-tree-file
    (let* ((str (string-trim (buffer-substring-no-properties
                              (point-min) (point-max))))
           (line-lst (split-string str "\n" t))
           (str-lst (mapcar (lambda (line-str)
                              (save-match-data
                                (string-match "- +" line-str)
                                (substring line-str (match-end 0))))
                            line-lst))
           (level-lst (mapcar (lambda (line-str)
                                (/ (string-match "-" line-str) 2))
                              line-lst))
           cons-lst)
      (dotimes (i (length str-lst))
        (setq cons-lst (append cons-lst (list (cons (nth i str-lst) (nth i level-lst))))))
      cons-lst)))

(defun md-wiki-all-pages ()
  (mapcar #'car (md-wiki-structures)))

(defun md-wiki--parents-sub-pairs (pairs page)
  "Return the pairs used for `md-wiki-parents'."
  (let* ((pos (seq-position pairs page
                            (lambda (pair page)
                              (string= (car pair) page)))))
    (reverse (seq-subseq pairs 0 pos))))

(defun md-wiki-parents (page)
  (let* ((pairs (md-wiki-structures))
         (level (cdr (assoc page pairs)))
         (sub-pairs (md-wiki--parents-sub-pairs pairs page))
         (parent-levels (number-sequence (1- level) 0 -1))
         cons-lst)
    (dolist (level parent-levels)
      (push (seq-find (lambda (pair)
                        (= level (cdr pair)))
                      sub-pairs)
            cons-lst))
    cons-lst))

(defun md-wiki-parents-pages (page)
  (mapcar #'car (md-wiki-parents page)))

(defun md-wiki--children-sub-pairs (pairs page)
  "Return the pairs used for `md-wiki-children'."
  (let* ((beg (seq-position pairs page
                            (lambda (pair page)
                              (string= (car pair) page))))
         (level (cdr (assoc page pairs)))
         (rest-pairs (seq-subseq pairs (1+ beg)))
         (end (seq-position rest-pairs level
                            (lambda (pair level)
                              (= (cdr pair) level)))))
    (seq-subseq rest-pairs 0 end)))

(defun md-wiki-children (page)
  (let* ((pairs (md-wiki-structures))
         (level (cdr (assoc page pairs)))
         (sub-pairs (md-wiki--children-sub-pairs pairs page)))
    (seq-filter (lambda (pair)
                  (= (cdr pair) (1+ level)))
                sub-pairs)))

(defun md-wiki-children-pages (page)
  (mapcar #'car (md-wiki-children page)))

(defun md-wiki-siblings (page)
  (when-let ((parents (md-wiki-parents page)))
    (md-wiki-children
     (car (car (last parents))))))

(defun md-wiki-siblings-pages (page)
  (mapcar #'car (md-wiki-siblings page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun md-wiki-insert-metas (page)
  (insert (format md-wiki-meta-fmt page)))

(defun md-wiki-metas (page)
  (format md-wiki-meta-fmt page))

(defun md-wiki-fmt-page-link (page slug)
  (format "[%s](%s)" page (concat md-wiki-prefix-url "/" slug)))

(defun md-wiki-page-slugfy (page)
  (downcase (string-join (split-string page "[ ]+" t) "-")))

(defun md-wiki-page-link (page)
  (md-wiki-fmt-page-link page (md-wiki-page-slugfy page)))

(defun md-wiki-join-page-link (pages separator)
  (string-join (mapcar (lambda (page)
                         (md-wiki-page-link page))
                       pages)
               separator))

(defun md-wiki-index-page-link ()
  (md-wiki-fmt-page-link md-wiki-index-page-shown
                         (md-wiki-page-slugfy md-wiki-index-page)))

(defun md-wiki-compose-parents-nav (page)
  (let* ((pages (md-wiki-parents-pages page))
         (index-link (md-wiki-index-page-link))
         (sep md-wiki-parents-nav-sep)
         (link-str (md-wiki-join-page-link pages sep))
         (html-wrapper md-wiki-parents-html-wrapper))
    (if pages
        (format html-wrapper (concat md-wiki-parents-nav-prefix index-link sep link-str sep page))
      (format html-wrapper (concat md-wiki-parents-nav-prefix index-link sep page)))))

(defun md-wiki-compose-children-nav (page)
  (let* ((pages (md-wiki-children-pages page))
         (sep md-wiki-children-nav-sep)
         (link-str (md-wiki-join-page-link pages sep))
         (html-wrapper md-wiki-children-html-wrapper))
    (when pages (format html-wrapper (concat md-wiki-children-nav-prefix link-str)))))

(defun md-wiki-insert-navs (page)
  (let* ((parents-str (md-wiki-compose-parents-nav page))
         (children-str (md-wiki-compose-children-nav page))
         (fmt1 (concat "---" "\n%s\n\n%s\n\n" "---"))
         (fmt2 (concat "---" "\n%s\n\n" "---")))
    (if children-str
        (insert (format fmt1 parents-str children-str))
      (insert (format fmt2 parents-str)))))

(defun md-wiki-page-new (page)
  "Create a new md-wiki page according to config."
  (with-file-buffer (md-wiki-page-file page)
    ;; Renew the deleted file, the content still exists? need to earse buffer.
    (erase-buffer)
    (insert (md-wiki-metas page))
    (insert "\n")
    (md-wiki-insert-navs page)
    (save-buffer)))

(defun md-wiki-page-force-renew ()
  (let ((pages (md-wiki-all-pages)))
    (mapcar #'md-wiki-page-delete pages)
    (mapcar #'md-wiki-page-new pages)))

(defun md-wiki-page-update-nav (page)
  "Update the nav of a md-wiki page."
  (with-file-buffer (md-wiki-page-file page)
    (when (re-search-forward "^---" nil t 3)
      (let ((beg (line-beginning-position))
            (end (if (re-search-forward "^---" nil t)
                     (point)
                   (error "error format of md-wiki nav"))))
        (delete-region beg end)
        (md-wiki-insert-navs page)))
    (save-buffer)))

(defun md-wiki-page-update-meta (page)
  "Update the meta of a md-wiki page."
  (with-file-buffer (md-wiki-page-file page)
    (when (re-search-forward "^---" nil t)
      (let ((beg (line-beginning-position))
            (end (if (re-search-forward "^---" nil t)
                     (point)
                   (error "error format of md-wiki meta"))))
        (delete-region beg end)
        (insert (md-wiki-metas page))))
    (save-buffer)))

(defun md-wiki-page-delete (page)
  "Delete the md-wiki page according to config."
  (delete-file (md-wiki-page-file page) t))

;;; Index Page

(defun md-wiki-bookmarks ()
  (string-join (mapcar (lambda (title)
                         (md-wiki-page-link title))
                       md-wiki-bookmark-list)
               md-wiki-bookmark-separator))

(defun md-wiki-page-content (title)
  (let ((content (file-contents (md-wiki-page-file title))))
    (save-match-data
      (if (string-match "---\\(\n.*\\)+\\(\n---\\)\\{2\\}\\(\n.*\\)+\n---" content)
          (substring-no-properties content (1+ (match-end 0)))
        (error "Invalid page header format: %s" title)))))

(defun md-wiki-page-content-linum (title)
  "Return the line number of content in page."
  (when (file-exists-p (md-wiki-page-content title))
    (with-temp-buffer
      (insert (md-wiki-page-content title))
      (count-lines (point-min) (point-max)))))

(defun md-wiki-index-item-list ()
  "Return a list of each line string for Index page."
  (mapcar (lambda (pair)
            (let ((title (car pair))
                  (level (cdr pair)))
              (format "%s- %s (%s)"
                      (format "%s" (make-string (* level 2) ? ))
                      (md-wiki-page-link title)
                      (or (md-wiki-page-content-linum title) 0))))
          (md-wiki-structures)))

(defun md-wiki-render-index ()
  "Generate wiki sitemap page"
  (interactive)
  (let* ((index-file (md-wiki-page-file md-wiki-index-page-shown))
         (str-lst (md-wiki-index-item-list))
         (index-str (string-join str-lst "\n")))
    (with-file-buffer index-file
      (erase-buffer)
      (insert (md-wiki-metas md-wiki-index-page-shown))
      (insert "\n---\n" md-wiki-bookmark-prefix (md-wiki-bookmarks) "\n\n---")
      (insert "\n" index-str)
      (save-buffer))))

;;; Common Pages

(defun md-wiki-gen-pages (diff &optional force-nav force-meta)
  "Generate wiki pages according to DIFF."
  (let ((add-pages (plist-get diff :add))
        (update-pages (plist-get diff :update))
        (delete-pages (plist-get diff :delete)))
    (message "add-pages:%s" add-pages)
    (when add-pages (mapcar #'md-wiki-page-new add-pages))
    (when update-pages (mapcar #'md-wiki-page-update-nav update-pages))
    (when delete-pages (mapcar #'md-wiki-page-delete delete-pages))
    (when force-nav (mapcar #'md-wiki-page-update-nav (md-wiki-all-pages)))
    (when force-meta (mapcar #'md-wiki-page-update-meta (md-wiki-all-pages)))))

(defun md-wiki-config-backup ()
  "Backup the config when gen pages "
  (with-file-buffer md-wiki-diff-file
    (let ((inhibit-read-only 1))
      (erase-buffer)
      (insert (format "%S" (md-wiki-structures)))
      (save-buffer)
      (read-only-mode 1))))

(defun md-wiki-config-diff ()
  "Diff the backup config and current config.
Return the pages need to add, update and delete."
  (let* ((_ (unless (file-exists-p md-wiki-diff-file)
              (with-file-buffer md-wiki-diff-file
                (insert "nil")
                (save-buffer))))
         (backup-config (file-contents md-wiki-diff-file))
         (old-cons-lst (car (read-from-string backup-config)))
         (curr-cons-lst (md-wiki-structures))
         (old-page-lst (mapcar #'car old-cons-lst))
         (curr-page-lst (md-wiki-all-pages))
         (add-pages (seq-difference curr-page-lst old-page-lst))
         (delete-pages (seq-difference old-page-lst curr-page-lst))
         (all-pages (seq-uniq (seq-concatenate 'list old-page-lst curr-page-lst)))
         (update-pages
          (seq-filter
           (lambda (page)
             (not (= (cdr (assoc page curr-cons-lst))
                     (cdr (assoc page old-cons-lst)))))
           (seq-difference all-pages (seq-concatenate 'list add-pages delete-pages)))))
    (list :add add-pages :delete delete-pages :update update-pages)))

(defun md-wiki-diff-p (diff)
  "Judge if current tree S has difference with tree S in `md-wiki-diff-file'."
  (or (plist-get diff :add)
      (plist-get diff :update)
      (plist-get diff :delete)))

;;;; Commands

;;;###autoload
(defun md-wiki-gen-site (&optional force-nav force-meta)
  (interactive)
  (let ((diff (md-wiki-config-diff)))
    (when force-nav (md-wiki-gen-pages diff t))
    (when force-meta (md-wiki-gen-pages diff nil t))
    (when (or force-nav force-meta) (md-wiki-render-index))
    (when (md-wiki-diff-p diff) (md-wiki-render-index))
    (unless (or force-nav force-meta)
      (md-wiki-gen-pages diff))
    ;; backup current config file to `md-wiki-diff-file'.
    (md-wiki-config-backup)))

;;;###autoload
(defun md-wiki-gen-site-force-nav ()
  (interactive)
  (md-wiki-gen-site t))

;;;###autoload
(defun md-wiki-gen-site-force-meta ()
  (interactive)
  (md-wiki-gen-site nil t))

(defun md-wiki-page-find (title)
  (find-file (md-wiki-page-file title))
  (goto-char (point-max)))

;;;###autoload
(defun md-wiki-page-edit ()
  (interactive)
  (let ((title (completing-read "Choose a page to edit: "
                                (append `((,md-wiki-index-page-shown))
                                        (md-wiki-structures)))))
    (md-wiki-page-find title)))

;;;###autoload
(defun md-wiki-tree-edit ()
  (interactive)
  (find-file md-wiki-tree-file))

;;;###autoload
(defun md-wiki-show-diff ()
  (interactive)
  (let ((diff (md-wiki-config-diff)))
    (message "add:%s | delete:%s | update:%s"
             (plist-get diff :add)
             (plist-get diff :delete)
             (plist-get diff :update))))

(defun md-wiki--browse-page (title)
  (browse-url (concat md-wiki-browse-url-base (md-wiki-page-slugfy title))))

;;;###autoload
(defun md-wiki-browse-page ()
  "Open page through browser."
  (interactive)
  (let ((title (completing-read "Choose a page to browse: "
                                (append (list (list md-wiki-index-page-shown))
                                        (md-wiki-structures)))))
    (md-wiki--browse-page title)))

;;;###autoload
(defun md-wiki-browse-curr-page ()
  "Open current page through browser"
  (interactive)
  (let ((title (md-wiki-curr-page-title)))
    (md-wiki--browse-page title)))

;;; Capture
(defvar md-wiki-return-window-conf nil
  "The window configuration of md-wiki before capturing a page.")
(defvar md-wiki-capture-page nil)
(defvar md-wiki-capture-buf "*Wiki Capture*")

(define-minor-mode md-wiki-capture-mode
  "Minor mode for md-wiki capture buffer."
  :global nil
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'md-wiki-capture-finish)
    (define-key map (kbd "C-c C-k") #'md-wiki-capture-cancel)
    map)
  (if md-wiki-capture-mode
      (setq-local header-line-format
                  (substitute-command-keys
                   "\\<md-wiki-capture-mode-map>Capture text, finish \
`\\[md-wiki-capture-finish]', cancel `\\[md-wiki-capture-cancel]'."))
    (setq-local header-line-format nil)))

(defun md-wiki-capture-finish ()
  (interactive)
  (let ((content (buffer-substring (point-min) (point-max))))
    (with-current-buffer (find-file-noselect
                          (md-wiki-page-file md-wiki-capture-page))
      (erase-buffer)
      (insert content)))
  (set-window-configuration md-wiki-return-window-conf)
  (kill-buffer md-wiki-capture-buf))

(defun md-wiki-capture-cancel ()
  (interactive)
  (set-window-configuration md-wiki-return-window-conf)
  (kill-buffer md-wiki-capture-buf)
  (setq md-wiki-return-window-conf nil)
  (setq md-wiki-capture-page nil))

;;;###autoload
(defun md-wiki-page-capture (&optional title)
  "Quick edit a page in a capture buffer."
  (interactive)
  (let* ((title (or title
                    (completing-read "Choose a page to edit: "
                                     (append `((,md-wiki-index-page-shown))
                                             (md-wiki-structures)))))
         (content (file-contents (md-wiki-page-file title))))
    (setq md-wiki-return-window-conf (current-window-configuration))
    (setq md-wiki-capture-page title)
    (switch-to-buffer (get-buffer-create md-wiki-capture-buf) nil)
    (insert content)
    (markdown-mode)
    (md-wiki-capture-mode 1)))

;;; wiki tree

;; (md-wiki-structures)
(require 'ewoc)
(defvar md-wiki-ewoc nil)
(defvar md-wiki-tree-mode-map nil)
(defvar md-wiki-tree-buffer "*Md-Wiki Tree*")

(defun md-wiki-tree-pp (cons)
  "Pretty printer of md-wiki tree."
  (let ((title (car cons))
        (level (cdr cons)))
    (insert (format "%s• %s" (make-string (* 2 level) ? )
                    (propertize title 'face 'font-lock-function-name-face)))))

(defun md-wiki-tree-buf-setup ()
  (let ((inhibit-read-only t))
    (kill-all-local-variables)
    (setq major-mode 'md-wiki-tree-mode
          mode-name "Wiki Tree")
    (use-local-map md-wiki-tree-mode-map)
    (erase-buffer)
    (buffer-disable-undo)))

(defun md-wiki-tree-buffer-render ()
  (switch-to-buffer (get-buffer-create md-wiki-tree-buffer))
  (md-wiki-tree-buf-setup)
  (md-wiki-tree-keymap-setup)
  (let ((ewoc (ewoc-create 'md-wiki-tree-pp
                           (propertize "MD-WIKI\n" 'face '(:height 1.5))
                           ;; (substitute-command-keys "\n\\{md-wiki-tree-mode-map}")
                           )))
    (set (make-local-variable 'md-wiki-ewoc) ewoc)
    (dolist (data (md-wiki-structures))
      (ewoc-enter-last ewoc data)))
  (read-only-mode 1))

;;;###autoload
(defun md-wiki-tree-show ()
  "Show the tree buffer of md-wiki."
  (interactive)
  (if-let ((buf (get-buffer md-wiki-tree-buffer)))
      (switch-to-buffer buf)
    (md-wiki-tree-buffer-render)))

;;;###autoload
(defun md-wiki-tree-refresh ()
  (interactive)
  (let ((inhibit-read-only t))
    (md-wiki-tree-buffer-render)
    (message "[md-wiki] wiki tree refreshed.")))

(defun md-wiki-ewoc-data ()
  "Return data at current ewoc position."
  (ewoc-data (ewoc-locate md-wiki-ewoc)))

(defun md-wiki-ewoc-title ()
  "Return page title at current ewoc position."
  (car (md-wiki-ewoc-data)))

;;;###autoload
(defun md-wiki-tree-page-edit ()
  "Enter the page at point."
  (interactive)
  (md-wiki-page-find (md-wiki-ewoc-title)))

;;;###autoload
(defun md-wiki-tree-page-capture ()
  (interactive)
  (md-wiki-page-capture (md-wiki-ewoc-title)))

(defun md-wiki-tree-keymap-setup ()
  (setq md-wiki-tree-mode-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)
          (define-key map (kbd "G") #'md-wiki-tree-refresh)
          (define-key map (kbd "<RET>") #'md-wiki-tree-page-edit)
          (define-key map (kbd "<S-return>") #'md-wiki-tree-page-capture)
          map)))

(provide 'md-wiki)
