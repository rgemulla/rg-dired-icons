;;; rg-dired-icons.el --- Dired minor mode that displays icons  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Rainer Gemulla
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;; Keywords: dired, icons
;; Package-Requires: ((f "20170404.1039") (emacs "24.4"))
;; URL: https://github.com/rgemulla/rg-dired-icons
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; See README.
;;
;; Based on all-the-icons-dired.el:
;; https://github.com/jtbm37/all-the-icons-dired
;;
;; To add support for additional operating systems / window managers to this
;; package, create a new file rg-dired-icons-<window-system>.el. The file needs
;; to provide the methods rg-dired-icons--ensure-external-programs,
;; rg-dired-icons--create-image-for-directory,
;; rg-dired-icons--create-image-for-executable-file,
;; rg-dired-icons--create-image-for-extension, and
;; rg-dired-icons--create-image-for-default-file. See rg-dired-icons-w32 for the
;; definition of these methods. All common functionality (such as file type
;; detection and caching) is handled in an OS-independent way.
;;
;;; Code:


;; -----------------------------------------------------------------------------
;; Icon size
;; -----------------------------------------------------------------------------

(defcustom rg-dired-icons-default-icon-size nil
  "Default icon size in pixels.
If left nil, icon size is determined by the current line pixel height."
  :type 'integer
  :group 'rg-dired-icons)

(defun rg-dired-icons-get-icon-size (&optional icon-size)
  "Determine the icon size to use.
If ICON-SIZE is nil, returns the default icon size based on the
setting of `rg-dired-icons-default-icon-size'.  Otherwise, returns
ICON-SIZE."
  (cond
   (icon-size icon-size)
   (rg-dired-icons-default-icon-size rg-dired-icons-default-icon-size)
   (t (line-pixel-height))))


;; -----------------------------------------------------------------------------
;; logging
;; -----------------------------------------------------------------------------

(defcustom rg-dired-icons-debug 'error
  "If error, log only errors.
Otherwise, if non-nil,
additionally log debug messages to the *Messages* buffer.  If
  nil, no messages."
  :group 'rg-dired-icons)


(defmacro rg-dired-icons--log (msg &optional type)
  "Logs the specified MSG with the specified TYPE (either nil or 'error).
Returns t when message was logged, else nil.  Respects ;
`rg-dired-icons-debug'."
  ;; determine whether to log
  `(when (and rg-dired-icons-debug
              (or (not (eq rg-dired-icons-debug 'error))
                  (eq ,type 'error)))
     ;; now determine how to log and log
     (if (and ,type
              (eq ,type 'error))
         (message "rg-dired-icons: Error: %s" ,msg)
       (let ((inhibit-message t))
         (message "rg-dired-icons: %s" ,msg)))
     t))


;; -----------------------------------------------------------------------------
;; Quoting and unquoting
;; -----------------------------------------------------------------------------

(defun rg-dired-icons--quote (string)
  "Surrounds STRING with quotes, if not already present."
  (if (string-match-p "^\".*\"$" string)
      string
    (concat "\"" string "\"")))

(defun rg-dired-icons--unquote (string)
  "Strips surrounding quotes from STRING, if present."
  (if (string-match "^\"\\(.*\\)\"$" string)
      (match-string 1 string)
    string))


;; -----------------------------------------------------------------------------
;; Load required and OS-specific packages
;; -----------------------------------------------------------------------------

(require 'dired)

;; caching functions
(require 'rg-dired-icons-cache)

;; load system-specific functions to create imanges
(cond
 ((eq (window-system) 'w32) (require 'rg-dired-icons-w32))
 (t (error (format "rg-dired-icons: window system %s currently not supported."
                   (symbol-name (window-system))))))


;; -----------------------------------------------------------------------------
;; Main entry points (OS-independent)
;; -----------------------------------------------------------------------------

(defun rg-dired-icons-create-image-for-directory (&optional icon-size)
  "Return an image of the directory icon of size ICON-SIZE."
  (let* ((cache-key (rg-dired-icons--cache-key "directory" icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons--create-image-for-directory icon-size cache-key)))))


(defun rg-dired-icons-create-image-for-executable-file (&optional icon-size)
  "Return an image of an executable file of size ICON-SIZE."
  (let* ((cache-key (rg-dired-icons--cache-key ".exe" icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons--create-image-for-executable-file icon-size cache-key)))))

(defun rg-dired-icons-create-image-for-extension (ext &optional icon-size)
  "Return an image of the icon associated with extension EXT  of size ICON-SIZE.
EXT should start with a dot."
  (let* ((cache-key (rg-dired-icons--cache-key ext icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons--create-image-for-extension ext icon-size cache-key)))))

(defun rg-dired-icons-create-image-for-default-file (&optional icon-size)
  "Return an image of the default file icon of size ICON-SIZE."
  (let* ((cache-key (rg-dired-icons--cache-key "default" icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons--create-image-for-default-file icon-size cache-key)))))

(defun rg-dired-icons-create-image-for-file (file &optional icon-size)
  "Return an image of the icon associated with the given FILE.
FILE can be a directory or it can be non-existing.  When no icon
is found, returns the default file icon.  Returned image has size
ICON-SIZE."
  (let* ((symlink (file-symlink-p file))
         (ext (if symlink
                  (file-name-extension symlink t)
                (file-name-extension file t)))
         (image))
    (cond
     ((file-directory-p file)
      (setq image (rg-dired-icons-create-image-for-directory icon-size)))
     ((and ext (equal ext ".exe"))
      (setq image (rg-dired-icons-create-image-for-executable-file icon-size)))
     ((and ext (not (equal ext "")))
      (setq image (rg-dired-icons-create-image-for-extension ext icon-size))))
    (unless image
      (setq image (rg-dired-icons-create-image-for-default-file icon-size))
      ;; store also misses with default icon to not try again (only in memory)
      (when (and ext (not (equal ext "")))
        (rg-dired-icons--store-image-in-cache (rg-dired-icons--cache-key ext icon-size) 'default)))
    image))


;; -----------------------------------------------------------------------------
;; Minor mode definition
;; -----------------------------------------------------------------------------

(defun rg-dired-icons--display ()
  "Display the icons of files in a dired buffer."
  (when dired-subdir-alist
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (dired-move-to-filename nil)
            ;; check whether there is an image on this line
            (unless (or (get-text-property (- (point) 2) 'display)
                        (equal (char-after (- (point) 2)) ??))
              (let ((file (dired-get-filename 'verbatim t)))
                (unless (member file '("." ".."))
                  (let* ((filename (dired-get-filename nil t))
                         (image (rg-dired-icons-create-image-for-file filename)))
                    (if image
                        (progn
                          (insert-image image)
                          (insert " "))
                      (insert "? ")))))))
            (forward-line 1))))))

(defun rg-dired-icons--hook ()
  "Hook to displays all icons that are missing."
  (rg-dired-icons--display))

;;;###autoload
(define-minor-mode rg-dired-icons-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " rg-dired-icons-mode"
  (rg-dired-icons--ensure-external-programs)
  (if (and (display-graphic-p) rg-dired-icons-mode)
      (progn
        (add-hook 'dired-after-readin-hook 'rg-dired-icons--hook t t)
        (when (derived-mode-p 'dired-mode)
          (rg-dired-icons--display)))
    (remove-hook 'dired-after-readin-hook 'rg-dired-icons--hook t)
    (dired-revert)))

(provide 'rg-dired-icons)

;;; rg-dired-icons.el ends here
