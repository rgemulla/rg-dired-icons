;;; rg-dired-icons-win.el -*- lexical-binding: t; -*-
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

(require 'f)


(defcustom rg-dired-icons-w32-imagemagick-directory ""
  "Directory where ImageMagick is installed.
Needs to be an absolute path and end with /.  Leave empty to use system search
  path instead."
  :type 'string
  :group 'rg-dired-icons)

(defcustom rg-dired-icons-w32-resource-hacker-directory ""
  "Directory where ResourceHacker is installed.
Needs to be an absolute path and end with /.  Leave empty to use system search
  path instead."
  :type 'string
  :group 'rg-dired-icons)


;; -----------------------------------------------------------------------------
;; Registry querying
;; -----------------------------------------------------------------------------

(defun rg-dired-icons-w32--reg-query (key-arg value-arg)
  "Query the Windows registry.
KEY-ARG and VALUE-ARG are used as arguments to \"reg query\".
Returns nil on error."
  (let ((result (shell-command-to-string (format "reg query %s %s" key-arg value-arg))))
    (when (and result (not (string-match-p "\\`ERROR" result)))
        result)))

(defun rg-dired-icons-w32--reg-entry-first-key (entry)
  "Extract the first value of type REG_SZ or REG_EXPAND_SZ from ENTRY.
ENTRY should be a result of
`rg-dired-icons-w32--reg-query'.  Returns nil on error."
  (save-match-data
    (when (and entry (string-match "^ * \\([^ ]+\\) *REG_" entry))
      (match-string 1 entry))))

(defun rg-dired-icons-w32--reg-entry-first-value (entry)
  "Extract the first value of type REG_SZ or REG_EXPAND_SZ from ENTRY.
ENTRY should be a result of
`rg-dired-icons-w32--reg-query'.  Returns nil on error."
  (let ((result
         (save-match-data
           (when (and entry (string-match "^.* +REG_\\(EXPAND_\\)?SZ +\\([^ ].*\\)$" entry))
             (match-string 2 entry)))))
    (when (and result (not (string-match-p "VALUE NOT SET" (upcase result))))
      result)))

(defun rg-dired-icons-w32--reg-entry-query-first-value (key-arg value-arg)
  "Query the Windows registry and extract the first value.
KEY-ARG and VALUE-ARG are used as arguments to \"reg query\".
Returns nil on error."
  (let ((entry (rg-dired-icons-w32--reg-query key-arg value-arg)))
    (when entry
      (rg-dired-icons-w32--reg-entry-first-value entry))))

(defun rg-dired-icons-w32--reg-entry-query-first-key (key-arg value-arg)
  "Query the Windows registry and extract the first key.
KEY-ARG and VALUE-ARG are used as arguments to \"reg query\".
Returns nil on error."
  (let ((entry (rg-dired-icons-w32--reg-query key-arg value-arg)))
    (when entry
      (rg-dired-icons-w32--reg-entry-first-key entry))))

(defun rg-dired-icons-w32--reg-get-progid-for-extension (ext)
  "Return the progid registered for extension EXT or nil when none."
  (let* ((progid (or (rg-dired-icons-w32--reg-entry-query-first-value
                      (format "HKCR\\%s\\Userchoice" ext) "/v ProgId")
                     (rg-dired-icons-w32--reg-entry-query-first-value
                      (format "HKCR\\%s" ext) "/ve")
                     (rg-dired-icons-w32--reg-entry-query-first-value
                      (format "HKCR\\%s\\FriendlyTypeName" ext) "/ve")
                     (rg-dired-icons-w32--reg-entry-query-first-value
                      (format "HKCR\\%s\\OpenWithProgids" ext) "/ve")
                     (rg-dired-icons-w32--reg-entry-query-first-key
                      (format "HKCR\\%s\\OpenWithProgids" ext) ""))))
    (if progid
        (rg-dired-icons--log (format "Found progid for extension %s: %s" ext progid))
      (rg-dired-icons--log (format "Did not find progid for extension %s" ext) 'error))
    progid
    ))

(defun rg-dired-icons-w32--reg-get-icon-resource-for-progid (progid)
  "Return the icon resource (of format \"filename,icon-number\") registered with PROGID or nil when not present."
  (let* ((entry (rg-dired-icons-w32--reg-query (format "HKCR\\%s\\DefaultIcon" progid) "/ve"))
         (command (unless entry (rg-dired-icons-w32--reg-entry-first-value
                                 (rg-dired-icons-w32--reg-query
                                  (format "HKCR\\%s\\Shell\\Open\\Command" progid)
                                  "/ve"))))
         (icon-resource (if entry (rg-dired-icons-w32--reg-entry-first-value entry)
                          (when (and command (string-match "^\"\\([^\"]+\\)\"" command))
                            (concat (match-string 1 command) ",0")))))
    (if icon-resource
        (rg-dired-icons--log
         (format "Found icon resource for progid %s: %s" progid icon-resource))
      (rg-dired-icons--log (format "Did not find icon resource for progid %s" progid) 'error))
    icon-resource))


;; -----------------------------------------------------------------------------
;; Icon extraction
;; -----------------------------------------------------------------------------

(defun rg-dired-icons--quote (string)
  "Surrounds STRING with quotes, if not already present."
  (if (string-match-p "^\".*\"$" string)
      string
    (concat "\"" string "\"")))

(defun rg-dired-icons--unquote (string)
  "Strips quotes from STRING, if present."
  (if (string-match "^\"\\(.*\\)\"$" string)
      (match-string 1 string)
    string))

(defun rg-dired-icons-w32--extract-icon-file (icon-resource)
  "Extracts the ico file from ICON-RESOURCE.
Returns nil on error."
  (let* ((file-n (split-string icon-resource ","))
         (file (rg-dired-icons--unquote (nth 0 file-n)))
         (file (if (and (not (file-exists-p file)) (not (string-match-p "[/|\\]" file)))
                   (concat "%WinDir%/System32/" file)
                 file))
         (n (int-to-string (abs (string-to-number (or (nth 1 file-n) "0")))))
         (ico-dir (concat rg-dired-icons-file-cache-directory (md5 (upcase file)) "/"))
         (rc-file (concat ico-dir "icon.rc"))
         (ico-file (concat ico-dir "Icon.ico"))
         (called-resource-hacker))
    ;; reuse extracted icons if present, else run Resource Hacker
    (if (file-exists-p ico-dir)
        (rg-dired-icons--log (format "Reusing extracted icons from file %s: %s" file ico-dir))
      (progn
        ;; create the directory and store information about its resource
        (make-directory ico-dir t)
        (with-temp-file (concat ico-dir "CONTENTS")
          (insert file))
        
        ;; try to extract icons
        (call-process shell-file-name ;; shell needed to handle expansions such as %systemroot%
                      nil nil nil
                      shell-command-switch
                      (format "%s -open %s -save %s -action extract -mask ICONGROUP,,"
                              (rg-dired-icons--quote (concat rg-dired-icons-w32-resource-hacker-directory
                                                       "ResourceHacker"))
                              (rg-dired-icons--quote file)
                              (rg-dired-icons--quote rc-file)))
        (setq called-resource-hacker t)))

    ;; determine the particular icon to use
    (let ((extracted-ico-file))
      (cond
       ((file-exists-p rc-file)
        ;; if rc-file is present, parse it to get ico file
        (with-temp-buffer
          (when called-resource-hacker
            (rg-dired-icons--log (format "Extracted icons from file %s: %s" file ico-dir)))
          (let ((coding-system-for-read 'utf-16le-with-signature))
            (insert-file-contents rc-file))
          (goto-char (point-min))
          (while (< (point) (point-max))
            (let ((line (thing-at-point 'line)))
              (when (and line
                         (string-match (concat "^" n " ICON \"\\(.*\\)\"$") line))
                (setq extracted-ico-file (concat ico-dir (match-string 1 line)))))
            (forward-line 1)))
        ;; when we did not find a matching item, use default one
        (unless extracted-ico-file
          (setq extracted-ico-file ico-file)))
       ;; if Icon.ico is present, use it
       ((file-exists-p ico-file)
        (setq extracted-ico-file ico-file))
       ;; if file is an ico file, use it
       ((equal (file-name-extension file t) ".ico")
        (progn
          (copy-file file ico-file t)
          (setq extracted-ico-file ico-file)
          (rg-dired-icons--log (format "Copied icon %s: %s" file ico-dir))))
       ;; otherwise, there is an error
       (t
        (rg-dired-icons--log
         (format "Could not extract icons from file %s" file) 'error)))

      (if extracted-ico-file
          (rg-dired-icons--log
           (format "Using icon from icon resource %s: %s" icon-resource extracted-ico-file))
        (rg-dired-icons--log
         (format "Could not determine icon in icon resource %s" icon-resource)
         'error))
      extracted-ico-file)))

(defun rg-dired-icons-w32--get-icon-frame (ico-file &optional icon-size)
  "Return the name of a suitable frame from ICO-FILE.
Finds the frame that best matches the specified
ICON-SIZE.  Returns nil on error."
  (let* ((icon-size (rg-dired-icons-get-icon-size icon-size))
         (best-frame) (best-size) (best-depth))
    (with-temp-buffer
      ;; get the frames
      (call-process
       (concat rg-dired-icons-w32-imagemagick-directory "identify")
       nil t nil
       ico-file)

      ;; parse the frames
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((line (thing-at-point 'line)))
          (when (and line
                     (string-match "\\(.*\\) ICO \\([[:digit:]]+\\)x.* \\([[:digit:]]+\\)-bit"
                                   line))
            (let* ((frame (match-string 1 line))
                   (size (string-to-number (match-string 2 line)))
                   (depth (string-to-number (match-string 3 line)))
                   (take (or (not best-size)
                             (and (< best-size icon-size) (> size best-size))
                             (and (> best-size icon-size) (>= size icon-size) (< size best-size))
                             (and (equal size best-size) (< best-depth depth)))))
              (rg-dired-icons--log
               (format "Found frame %s, size %sx%s, depth %s" frame size size depth))
              (when take
                (setq best-frame frame)
                (setq best-size size)
                (setq best-depth depth))
              )))
        (forward-line 1)))
    (if best-frame
        (rg-dired-icons--log (format "Selected frame: %s" best-frame))
      (rg-dired-icons--log (format "Could not select frame for icon file %s" ico-file) 'error))
    best-frame))


;; -----------------------------------------------------------------------------
;; Image creation
;; -----------------------------------------------------------------------------

(defun rg-dired-icons-w32--size-format-string (&optional icon-size)
  "Format string to use with ImageMagick convert.
Uses the specified ICON-SIZE."
  (let ((icon-size (if icon-size icon-size rg-dired-icons-default-icon-size)))
    (concat (int-to-string icon-size) "x" (int-to-string icon-size))))

(defun rg-dired-icons-w32--create-image-from-ico-file (ico-file &optional icon-size cache-key)
  "Create an image from ICO-FILE.
Return the image of the frame that best matches the specified
ICON-SIZE in the given ico-file.  When CACHE-KEY is non-nil,
stores the png image under name CACHE-KEY.png in
`rg-dired-icons-file-cache-directory'.  Returns nil on error."
  (when (file-exists-p ico-file)
    (let ((icon-size  (rg-dired-icons-get-icon-size icon-size))
          (frame (rg-dired-icons-w32--get-icon-frame ico-file icon-size))
          (png-file (if cache-key
                        (concat rg-dired-icons-file-cache-directory cache-key ".png")
                      (let ((temporary-file-directory rg-dired-icons-file-cache-directory))
                        (make-temp-file "rg-dired-icons-" nil ".png")))))
      (call-process
       (concat rg-dired-icons-w32-imagemagick-directory "convert")
       nil nil nil
       (concat "ico:" frame) "-resize" (rg-dired-icons-w32--size-format-string icon-size)
       "-depth" "32" ;; required to handle transparency well in Emacs
       (concat "png:" png-file))
      (if (and (file-exists-p png-file) (> (nth 7 (file-attributes png-file)) 0))
          (progn
            (rg-dired-icons--log (format "Created png file: %s" png-file))
            (create-image (f-read-bytes png-file) 'png t :ascent 'center :mask 'heuristic))
        (progn
          (rg-dired-icons--log (format "Could not create png file from %s" frame) 'error)
          nil)))))

(defun rg-dired-icons-w32--create-image-for-extension (ext &optional icon-size cache-key)
  "Create an image for the icon associated with extension EXT.
Return the image of size ICON-SIZE.  EXT should start with a
dot.  When CACHE-KEY is non-nil, stores the png image under name
CACHE-KEY.png in `rg-dired-icons-file-cache-directory'."
  (let* ((icon-size     (rg-dired-icons-get-icon-size icon-size))
         (progid        (rg-dired-icons-w32--reg-get-progid-for-extension ext))
         (icon-resource (when progid
                          (rg-dired-icons-w32--reg-get-icon-resource-for-progid progid)))
         (icon-file     (when icon-resource
                          (rg-dired-icons-w32--extract-icon-file icon-resource)))
         (image         (when icon-file
                          (rg-dired-icons-w32--create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg-dired-icons-w32--create-image-for-directory (&optional icon-size cache-key)
  "Create an image of the directory icon with the specified ICON-SIZE.
When CACHE-KEY is non-nil, stores the png image under name
CACHE-KEY.png in `rg-dired-icons-file-cache-directory'."
  (let* ((icon-resource "%windir%/system32/shell32.dll,4")
         (icon-file (rg-dired-icons-w32--extract-icon-file icon-resource))
         (image (when icon-file
                  (rg-dired-icons-w32--create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg-dired-icons-w32--create-image-for-default-file (&optional icon-size cache-key)
  "Create an image of the default file icon with the specified ICON-SIZE.
When CACHE-KEY is non-nil, stores the png image under name
CACHE-KEY.png in `rg-dired-icons-file-cache-directory'."
  (let* ((icon-resource "%windir%/system32/shell32.dll,1")
         (icon-file (rg-dired-icons-w32--extract-icon-file icon-resource))
         (image (when icon-file
                  (rg-dired-icons-w32--create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg-dired-icons-w32--create-image-for-executable-file (&optional icon-size cache-key)
  "Create an image of the default icon for executables.
Uses the specified ICON-SIZE.  When CACHE-KEY is non-nil, stores
the png image under name CACHE-KEY.png in
`rg-dired-icons-file-cache-directory'."
  (let* ((icon-resource "%SystemRoot%/System32/shell32.dll,3")
         (icon-file (rg-dired-icons-w32--extract-icon-file icon-resource))
         (image (when icon-file
                  (rg-dired-icons-w32--create-image-from-ico-file icon-file icon-size cache-key))))
    image))


;; -----------------------------------------------------------------------------
;; Minor entry points (OS-dependent)
;; -----------------------------------------------------------------------------

(defun rg-dired-icons-w32-create-image-for-directory (&optional icon-size)
  "Return an image of the directory icon of size ICON-SIZE."
  (let* ((cache-key (rg-dired-icons--cache-key "directory" icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons-w32--create-image-for-directory icon-size cache-key)))))


(defun rg-dired-icons-w32-create-image-for-default-file (&optional icon-size)
  "Return an image of the default file icon of size ICON-SIZE."
  (let* ((cache-key (rg-dired-icons--cache-key "default" icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons-w32--create-image-for-default-file icon-size cache-key)))))

(defun rg-dired-icons-w32-create-image-for-executable-file (&optional icon-size)
  "Return an image of an executable file of size ICON-SIZE."
  (let* ((cache-key (rg-dired-icons--cache-key ".exe" icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons-w32--create-image-for-executable-file icon-size cache-key)))))

(defun rg-dired-icons-w32-create-image-for-extension (ext &optional icon-size)
  "Return an image of the icon associated with extension EXT  of size ICON-SIZE.
EXT should start with a dot."
  (let* ((cache-key (rg-dired-icons--cache-key ext icon-size))
         (cached-image (rg-dired-icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg-dired-icons--store-image-in-cache
       cache-key
       (rg-dired-icons-w32--create-image-for-extension ext icon-size cache-key)))))


;; -----------------------------------------------------------------------------
;; Main entry points (OS-independent)
;; -----------------------------------------------------------------------------

(defun rg-dired-icons-ensure-external-programs ()
  "Check whether all required external programs are present."
  (unless (executable-find "reg")
    (error "rg-dired-icons: reg executable not found. Are you on Windws?"))
  (unless (executable-find (concat rg-dired-icons-w32-imagemagick-directory "convert"))
    (error "rg-dired-icons: ImageMagick's convert exectutable not found. Is rg-dired-icons-w32-imagemagick-directory set correctly?."))
  (unless (executable-find (concat rg-dired-icons-w32-imagemagick-directory "identify"))
    (error "rg-dired-icons: ImageMagick's identify exectutable not found. Is rg-dired-icons-w32-imagemagick-directory set correctly?."))
  (unless (executable-find (concat rg-dired-icons-w32-resource-hacker-directory "ResourceHacker"))
    (error "rg-dired-icons: ResourceHacker executable not found. Is rg-dired-icons-w32-resource-hacker-directory set correctly?.")))

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
      (setq image (rg-dired-icons-w32-create-image-for-directory icon-size)))
     ((and ext (equal ext ".exe"))
      (setq image (rg-dired-icons-w32-create-image-for-executable-file icon-size)))
     ((and ext (not (equal ext "")))
      (setq image (rg-dired-icons-w32-create-image-for-extension ext icon-size))))
    (unless image
      (setq image (rg-dired-icons-w32-create-image-for-default-file icon-size))
      ;; store also misses with default icon to not try again (only in memory)
      (when (and ext (not (equal ext "")))
        (rg-dired-icons--store-image-in-cache (rg-dired-icons--cache-key ext icon-size) 'default)))
    image))


(provide 'rg-dired-icons-w32)

;;; rg-dired-icons.el ends here
