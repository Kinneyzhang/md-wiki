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

(defun md-wiki-index-item-list ()
  (mapcar (lambda (pair)
            (let ((title (car pair))
                  (level (cdr pair)))
              (concat (format "%s- " (make-string (* level 2) ? ))
                      (md-wiki-page-link title))))
          (md-wiki-structures)))

(defvar md-wiki-bookmark-list
  '("Daily Page" "费曼学习法" "Emacs"))

(defvar md-wiki-bookmark-separator " / ")

(defvar md-wiki-bookmark-prefix "快捷访问> ")

(defun md-wiki-bookmarks ()
  (string-join (mapcar (lambda (title)
                         (md-wiki-page-link title))
                       md-wiki-bookmark-list)
               md-wiki-bookmark-separator))

(defun md-wiki-render-index ()
  "Generate wiki sitemap page"
  (let* ((index-file (md-wiki-page-file md-wiki-index-page-shown))
         (str-lst (md-wiki-index-item-list))
         (index-str (string-join str-lst "\n")))
    (with-file-buffer index-file
      (erase-buffer)
      (insert (md-wiki-metas md-wiki-index-page-shown))
      (insert "\n---\n" md-wiki-bookmark-prefix (md-wiki-bookmarks) "\n\n---")
      (insert "\n" index-str)
      (save-buffer))))

(defun md-wiki-gen-pages (diff &optional force-nav force-meta)
  "Generate wiki pages according to DIFF."
  (let ((add-pages (plist-get diff :add))
        (update-pages (plist-get diff :update))
        (delete-pages (plist-get diff :delete)))
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

;;;###autoload
(defun md-wiki-page-edit ()
  (interactive)
  (let ((page (completing-read "Choose a page to edit: "
                               (md-wiki-structures))))
    (find-file (md-wiki-page-file page))
    (goto-char (point-max))))

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

;;;###autoload
(defun md-wiki-browse-page ()
  "Open page through browser."
  (interactive)
  (let* ((page (completing-read "Choose a page to browse: "
                                (append (list (list md-wiki-index-page-shown))
                                        (md-wiki-structures))))
         (url (concat md-wiki-browse-url-base (md-wiki-page-slugfy page))))
    (browse-url url)))


(provide 'md-wiki)
