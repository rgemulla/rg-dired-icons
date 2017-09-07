;;; rg-dired-icons-cache.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Rainer Gemulla
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org-licenses/>.
;;
;;
;;; Commentary:
;; 

;;; Code:

(defcustom rg-dired-icons-file-cache-directory
  (expand-file-name (concat user-emacs-directory "rg-dired-icons/cache/"))
  "Directory where extracted files are stored and cached.
Must end with /.  Note that this directory is deleted completely
  using `rg-dired-icons-clear-file-cache', so don't store anything in
  there."
  :type 'string
  :group 'rg-dired-icons)

(when (not (file-directory-p rg-dired-icons-file-cache-directory))
  (make-directory rg-dired-icons-file-cache-directory t))

(defcustom rg-dired-icons-persistent-cache-file
  (expand-file-name (concat user-emacs-directory "rg-dired-icons-cache.el"))
  "Whether and where to persist the in-memory cache.
If non-nil, in memory-cache is persisted between Emacs sessions
in the file specified here.  If nil, the in-memory cache is not
persisted.  If you change this value, do so before loading this
package."
  :type 'string
  :group 'rg-dired-icons)

(defvar rg-dired-icons-cache nil
  "In-memory cache of icons.
Key is the caching key (typically of
form \"<extension>-<size>\"). Value is either an image or the
  symbol 'default.")

(defun rg-dired-icons--cache-key (base-key icon-size)
  "Compute a cache key from a BASE-KEY and an ICON-SIZE."
  (let ((icon-size (rg-dired-icons-get-icon-size icon-size)))
    (concat base-key "-" (int-to-string icon-size))))

(defun rg-dired-icons--retrieve-image-from-cache (cache-key)
  "Retrieves an image from the in-memory or the on-disk cache, in that order.
Returns nil if there is no entry with CACHE-KEY."
  (when cache-key
    (let ((entry (gethash cache-key rg-dired-icons-cache)))
      (cond
       ((eq entry 'default) (rg-dired-icons-create-image-for-default-file))
       (entry entry)
       (t (let ((cached-png-file
                 (concat rg-dired-icons-file-cache-directory cache-key ".png")))
            (when (file-exists-p cached-png-file)
              (rg-dired-icons--log (format "Reusing png file: %s" cached-png-file))
              (rg-dired-icons--store-image-in-cache
               cache-key
               (create-image (f-read-bytes cached-png-file) 'png t
                             :ascent 'center :mask 'heuristic)))))))))

(defun rg-dired-icons--store-image-in-cache (cache-key image)
  "Create in the cache entry CACHE-KEY with value IMAGE.
No action is performed when IMAGE is nil."
  (when image
    (rg-dired-icons--log (format "Caching image for key: %s" cache-key))
    (puthash cache-key image rg-dired-icons-cache))
  image)

(defun rg-dired-icons-clear-memory-cache ()
  "Clear the in-memory icon cache."
  (interactive)
  (clrhash rg-dired-icons-cache))

(defun rg-dired-icons-clear-file-cache ()
  "Clear the on-disk file cache."
  (interactive)
  (let ((delete-by-moving-to-trash nil))
    (delete-directory rg-dired-icons-file-cache-directory t t))
  (make-directory rg-dired-icons-file-cache-directory t))

(defun rg-dired-icons-clear-cache ()
  "Clear the in-memory and file cache."
  (interactive)
  (rg-dired-icons-clear-file-cache)
  (rg-dired-icons-clear-memory-cache))


(defun rg-dired-icons--save-memory-cache ()
  "Save the in-memory cache."
  (when (and rg-dired-icons-persistent-cache-file
             (file-writable-p rg-dired-icons-persistent-cache-file))
    (rg-dired-icons--log (format "Saving in-memory cache to: %s" rg-dired-icons-persistent-cache-file))
    (with-temp-file rg-dired-icons-persistent-cache-file
      (set-buffer-file-coding-system 'binary t)
      (insert (let (print-length) (prin1-to-string rg-dired-icons-cache))))))

(defun rg-dired-icons--read-memory-cache ()
  "Restore the in-memory cache."
  (when rg-dired-icons-persistent-cache-file
    (if (file-exists-p rg-dired-icons-persistent-cache-file)
        (with-demoted-errors
            "rg-dired-icons: Error: Failed to restore in-memory cache from: %S"
          (with-temp-buffer
              (insert-file-contents-literally rg-dired-icons-persistent-cache-file)
              (setq rg-dired-icons-cache (read (buffer-string))))
          (rg-dired-icons--log (format "In-memory-cache restored from file: %s"
                                 rg-dired-icons-persistent-cache-file)))
      (rg-dired-icons--log
       (format "File for restoring in memory-cache not found: %s"
               rg-dired-icons-persistent-cache-file) 'error)))
  rg-dired-icons-cache)

;; restore the cache from last time
(unless (rg-dired-icons--read-memory-cache)
  (setq rg-dired-icons-cache (make-hash-table :test 'equal)))

;; auto-save on exit
(add-hook 'kill-emacs-hook 'rg-dired-icons--save-memory-cache)

(provide 'rg-dired-icons-cache)
