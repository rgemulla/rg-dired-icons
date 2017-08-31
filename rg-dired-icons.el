;;; rg-dired-icons.el --- Dired minor mode that displays icons  -*- lexical-binding: t; -*-
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Based on all-the-icons-dired.el from jtbm37
;; https://github.com/jtbm37/all-the-icons-dired

(require 'dired)
(require 'rg-icons)

(defvar-local rg/dired-icons-displayed nil
  "Flags whether icons have been added.")

(defun rg/dired-icons--display ()
  "Display the icons of files in a dired buffer."
  (when (and (not rg/dired-icons-displayed) dired-subdir-alist)
    (setq-local rg/dired-icons-displayed t)
    (let ((inhibit-read-only t))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (when (dired-move-to-filename nil)
	    (let ((file (dired-get-filename 'verbatim t)))
	      (unless (member file '("." ".."))
		(let ((filename (dired-get-filename nil t)))
                  (insert-image (rg/icons-create-image-for-file filename))
                  (insert " ")))))
	  (forward-line 1))))))

(defun rg/dired-icons--reset (&optional _arg _noconfirm)
  "Functions used as advice when redisplaying buffer."
  (setq-local rg/dired-icons-displayed nil))

(defun rg/dired-icons--hook ()
  (setq-local rg/dired-icons-displayed nil)
  (rg/dired-icons--display))

;;;###autoload
(define-minor-mode rg/dired-icons-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " rg/dired-icons-mode"
  (if (and (display-graphic-p) rg/dired-icons-mode)
      (progn
        (add-hook 'dired-after-readin-hook 'rg/dired-icons--hook t t)
        (when (derived-mode-p 'dired-mode)
          (rg/dired-icons--display)))
    (remove-hook 'dired-after-readin-hook 'rg/dired-icons--hook t)
    (dired-revert)))

(advice-add 'dired-revert :before #'rg/dired-icons--reset)

(provide 'rg-dired-icons)
;;; rg/dired-icons.el ends here
