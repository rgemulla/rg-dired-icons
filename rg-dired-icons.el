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
;; To add support for different operating systems / window managers to this
;; package, create a new file rg-dired-icons-<window-system>. You file needs to
;; provide at least the methods rg-dired-icons-ensure-external-programs and
;; rg-dired-icons-create-image-for-file. See rg-dired-icons-win for the
;; definition of these methods.
;;
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
  (list 'when
        (list 'and
              'rg-dired-icons-debug
              (list 'or
                    (list 'not (list 'eq 'rg-dired-icons-debug ''error))
                    (list 'eq type ''error)))
        ;; now determine how to log and log
        (list 'if
              (list 'and type (list 'eq type ''error))
              (list 'message "rg-dired-icons: Error: %s" msg)
              (list 'let
                    (list (list 'inhibit-message t))
                    (list 'message "rg-dired-icons: %s" msg)))
        t))


;; -----------------------------------------------------------------------------
;; Required packages
;; -----------------------------------------------------------------------------

(require 'dired)

;; caching functions
(require 'rg-dired-icons-cache)

;; load system-specific functions to create imanges
(cond
 ((eq (window-system) 'w32) (require 'rg-dired-icons-w32))
 (t (error (format "rg-dired-icons: window system %s currently not supported." (symbol-name (window-system))))))


;; -----------------------------------------------------------------------------
;; Minor mode definition
;; -----------------------------------------------------------------------------

(defvar-local rg-dired-icons-displayed nil
  "Flags whether icons have been added.")

(defun rg-dired-icons--display ()
  "Display the icons of files in a dired buffer."
  (when (and (not rg-dired-icons-displayed) dired-subdir-alist)
    (setq-local rg-dired-icons-displayed t)
    (let ((inhibit-read-only t))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (when (dired-move-to-filename nil)
	    (let ((file (dired-get-filename 'verbatim t)))
	      (unless (member file '("." ".."))
		(let* ((filename (dired-get-filename nil t))
                       (image (rg-dired-icons-create-image-for-file filename)))
                  (if image
                      (progn
                        (insert-image image)
                        (insert " "))
                    (insert "? "))))))
	  (forward-line 1))))))

(defun rg-dired-icons--reset (&optional _arg _noconfirm)
  "Functions used as advice when redisplaying buffer."
  (setq-local rg-dired-icons-displayed nil))

(defun rg-dired-icons--hook ()
  "Hook to displays icons for a newly inserted subdirectory."
  (setq-local rg-dired-icons-displayed nil)
  (rg-dired-icons--display))

;;;###autoload
(define-minor-mode rg-dired-icons-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " rg-dired-icons-mode"
  (rg-dired-icons-ensure-external-programs)
  (if (and (display-graphic-p) rg-dired-icons-mode)
      (progn
        (add-hook 'dired-after-readin-hook 'rg-dired-icons--hook t t)
        (when (derived-mode-p 'dired-mode)
          (rg-dired-icons--display)))
    (remove-hook 'dired-after-readin-hook 'rg-dired-icons--hook t)
    (dired-revert)))

(advice-add 'dired-revert :before #'rg-dired-icons--reset)

(provide 'rg-dired-icons)

;;; rg-dired-icons.el ends here
