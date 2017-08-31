;;; rg-icons.el --- Retrieve icons for file associations  -*- lexical-binding: t; -*-
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

(require 'f)

(defcustom rg/icons-default-icon-size nil
  "Default icon size in pixels. If left nil, icon size is
  determined by the current line pixel height."
  :type 'integer
  :group 'rg-icons)

(defcustom rg/icons-imagemagick-directory ""
  "Directory where ImageMagick is installed. Needs to be an
  absolute path and end with /. Leave empty to use system search
  path instead."
  :type 'string
  :group 'rg-icons)

(defcustom rg/icons-resource-hacker-directory ""
  "Directory where ResourceHacker is installed. Needs to be an
  absolute path and end with /. Leave empty to use system search
  path instead."
  :type 'string
  :group 'rg-icons)

(defcustom rg/icons-debug 'error
  "If error, log only errors. Otherwise, if non-nil,
  additionally log debug messages to the *Messages* buffer. If
  nil, no messages."
  :group 'rg-icons)

(defcustom rg/icons-file-cache-directory
  (expand-file-name (concat user-emacs-directory "rg-icons/cache/"))
  "Directory where extracted files are stored and cached. Must
  end with /. Note that this directory is deleted completely
  using `rg/icons-clear-file-cache', so don't store anything in
  there."
  :type 'string
  :group 'rg-icons)

(when (not (file-directory-p rg/icons-file-cache-directory))
  (make-directory rg/icons-file-cache-directory t))

(defcustom rg/icons-persistent-cache-file
  (expand-file-name (concat user-emacs-directory "rg-icons-cache.el"))
  "If non-nil, in memory-cache is persisted between Emacs
  sessions in the file specified here. If nil, the in-memory
  cache is not persisted. If you change this value, do so before
  loading this package."
  :type 'string
  :group 'rg-icons)

(defvar rg/icons-cache nil
  "In-memory cache of icons. Key is the caching key (typically of
  form \"<extension>-<size>\"). Value is either an image or the
  symbol 'default.")

;; -----------------------------------------------------------------------------
;; logging
;; -----------------------------------------------------------------------------

(defmacro rg/icons--log (msg &optional type)
  "Logs the specified msg with the specified type (either nil or
'error). Returns t when message was logged, else nil. Respects ;
`rg/icons-debug'."
  ;; determine whether to log
  (list 'when
        (list 'and
              'rg/icons-debug
              (list 'or
                    (list 'not (list 'eq 'rg/icons-debug ''error))
                    (list 'eq type ''error)))
        ;; now determine how to log and log
        (list 'if
              (list 'and type (list 'eq type ''error))
              (list 'message "rg-icons: Error: %s" msg)
              (list 'let
                    (list (list 'inhibit-message t))
                    (list 'message "rg-icons: %s" msg)))
        t))


;; -----------------------------------------------------------------------------
;; Icon size
;; -----------------------------------------------------------------------------

(defun rg/icons-get-icon-size (&optional icon-size)
  "If ICON-SIZE is nil, returns the default icon size based on
the setting of `rg/icons-default-icon-size'. Otherwise, returns
ICON-SIZE."
  (cond
   (icon-size icon-size)
   (rg/icons-default-icon-size rg/icons-default-icon-size)
   (t (line-pixel-height))))

(defun rg/icons--size-format-string (&optional icon-size)
  "Format string to use with ImageMagick convert."
  (let ((icon-size (if icon-size icon-size rg/icons-default-icon-size)))
    (concat (int-to-string icon-size) "x" (int-to-string icon-size))))


;; -----------------------------------------------------------------------------
;; Registry querying
;; -----------------------------------------------------------------------------

(defun rg/icons--windows-reg-query (key-arg value-arg)
  "Queries the Windows registry using \"reg query\" with
  arguments KEY-ARG and VALUE-ARG. Returns nil on error."
  (let ((result (shell-command-to-string (format "reg query %s %s" key-arg value-arg))))
    (when (and result (not (string-match-p "\\`ERROR" result)))
        result)))

(defun rg/icons--windows-reg-entry-first-key (entry)
  "Extracts the first value of type REG_SZ or REG_EXPAND_SZ from
a result of `rg/icons--windows-reg-query'."
  (save-match-data
    (when (and entry (string-match "^ * \\([^ ]+\\) *REG_" entry))
      (match-string 1 entry))))

(defun rg/icons--windows-reg-entry-first-value (entry)
  "Extracts the first value of type REG_SZ or REG_EXPAND_SZ from
a result of `rg/icons--windows-reg-query'."
  (let ((result 
         (save-match-data
           (when (and entry (string-match "^.* +REG_\\(EXPAND_\\)?SZ +\\([^ ].*\\)$" entry))
             (match-string 2 entry)))))
    (when (and result (not (string-match-p "VALUE NOT SET" (upcase result))))
      result)))

(defun rg/icons--windows-reg-entry-query-first-value (key-arg value-arg)
  (let ((entry (rg/icons--windows-reg-query key-arg value-arg)))
    (when entry
      (rg/icons--windows-reg-entry-first-value entry))))

(defun rg/icons--windows-reg-entry-query-first-key (key-arg value-arg)
  (let ((entry (rg/icons--windows-reg-query key-arg value-arg)))
    (when entry
      (rg/icons--windows-reg-entry-first-key entry))))

(defun rg/icons--windows-reg-get-progid-for-extension (ext)
  "Returns the progid registered for the given extension or nil
when none."
  (let* ((progid (or (rg/icons--windows-reg-entry-query-first-value
                      (format "HKCR\\%s\\Userchoice" ext) "/v ProgId")
                     (rg/icons--windows-reg-entry-query-first-value
                      (format "HKCR\\%s" ext) "/ve")
                     (rg/icons--windows-reg-entry-query-first-value
                      (format "HKCR\\%s\\FriendlyTypeName" ext) "/ve")
                     (rg/icons--windows-reg-entry-query-first-value
                      (format "HKCR\\%s\\OpenWithProgids" ext) "/ve")
                     (rg/icons--windows-reg-entry-query-first-key
                      (format "HKCR\\%s\\OpenWithProgids" ext) ""))))
    (if progid
        (rg/icons--log (format "Found progid for extension %s: %s" ext progid))
      (rg/icons--log (format "Did not find progid for extension %s" ext) 'error))
    progid
    ))

(defun rg/icons--windows-reg-get-icon-resource-for-progid (progid)
  "Returns the icon resource (of format \"filename,icon-number\")
registered with the given progid or nil when not present."
  (let* ((entry (rg/icons--windows-reg-query (format "HKCR\\%s\\DefaultIcon" progid) "/ve"))
         (command (unless entry (rg/icons--windows-reg-entry-first-value
                                 (rg/icons--windows-reg-query
                                  (format "HKCR\\%s\\Shell\\Open\\Command" progid)
                                  "/ve"))))
         (icon-resource (if entry (rg/icons--windows-reg-entry-first-value entry)
                          (when (and command (string-match "^\"\\([^\"]+\\)\"" command))
                            (concat (match-string 1 command) ",0")))))
    (if icon-resource
        (rg/icons--log
         (format "Found icon resource for progid %s: %s" progid icon-resource))
      (rg/icons--log (format "Did not find icon resource for progid %s" progid) 'error))
    icon-resource))


;; -----------------------------------------------------------------------------
;; Icon extraction
;; -----------------------------------------------------------------------------

(defun rg/icons--quote (string)
  "Surrounds string with quotes, if not already present."
  (if (string-match-p "^\".*\"$" string)
      string
    (concat "\"" string "\"")))

(defun rg/icons--unquote (string)
  "Strips quotes from string, if present."
  (if (string-match "^\"\\(.*\\)\"$" string)
      (match-string 1 string)
    string))

(defun rg/icons--windows-extract-icon-file (icon-resource)
  "Extracts the ico file for the given icon resource. Returns nil
on error."
  (let* ((file-n (split-string icon-resource ","))
         (file (rg/icons--unquote (nth 0 file-n)))
         (file (if (and (not (file-exists-p file)) (not (string-match-p "[/|\\]" file)))
                   (concat "%WinDir%/System32/" file)
                 file))
         (n (int-to-string (abs (string-to-number (or (nth 1 file-n) "0")))))
         (ico-dir (concat rg/icons-file-cache-directory (md5 (upcase file)) "/"))
         (rc-file (concat ico-dir "icon.rc"))
         (ico-file (concat ico-dir "Icon.ico"))
         (called-resource-hacker))
    ;; reuse extracted icons if present, else run Resource Hacker
    (if (file-exists-p ico-dir)
        (rg/icons--log (format "Reusing extracted icons from file %s: %s" file ico-dir))
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
                              (rg/icons--quote (concat rg/icons-resource-hacker-directory
                                                       "ResourceHacker"))
                              (rg/icons--quote file)
                              (rg/icons--quote rc-file)))
        (setq called-resource-hacker t)))

    ;; determine the particular icon to use
    (let ((extracted-ico-file))
      (cond
       ((file-exists-p rc-file)
        ;; if rc-file is present, parse it to get ico file
        (with-temp-buffer
          (when called-resource-hacker
            (rg/icons--log (format "Extracted icons from file %s: %s" file ico-dir)))
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
          (rg/icons--log (format "Copied icon %s: %s" file ico-dir))))
       ;; otherwise, there is an error
       (t
        (rg/icons--log
         (format "Could not extract icons from file %s" file) 'error)))

      (if extracted-ico-file
          (rg/icons--log
           (format "Using icon from icon resource %s: %s" icon-resource extracted-ico-file))
        (rg/icons--log
         (format "Could not determine icon in icon resource %s" icon-resource)
         'error))
      extracted-ico-file)))

(defun rg/icons--get-icon-frame (ico-file &optional icon-size)
  "Returns the name of the frame that best matches the specified
icon size in the given ico file. Returns nil on error."
  (let* ((icon-size (rg/icons-get-icon-size icon-size))
         (best-frame) (best-size) (best-depth))
    (with-temp-buffer
      ;; get the frames
      (call-process
       (concat rg/icons-imagemagick-directory "identify")
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
              (rg/icons--log
               (format "Found frame %s, size %sx%s, depth %s" frame size size depth))
              (when take
                (setq best-frame frame)
                (setq best-size size)
                (setq best-depth depth))
              )))
        (forward-line 1)))
    (if best-frame
        (rg/icons--log (format "Selected frame: %s" best-frame))
      (rg/icons--log (format "Could not select frame for icon file %s" ico-file) 'error))
    best-frame))


;; -----------------------------------------------------------------------------
;; Image creation
;; -----------------------------------------------------------------------------

(defun rg/icons--create-image-from-ico-file (ico-file &optional icon-size cache-key)
  "Returns an image of the frame that best matches the specified
icon size in the given ico file. When cache-ext is non-nil,
stores the png image under name CACHE-KEY.png in
`rg/icons-file-cache-directory'. Returns nil on error."
  (when (file-exists-p ico-file)
    (let ((icon-size  (rg/icons-get-icon-size icon-size))
          (frame (rg/icons--get-icon-frame ico-file icon-size))
          (png-file (if cache-key
                        (concat rg/icons-file-cache-directory cache-key ".png")
                      (let ((temporary-file-directory rg/icons-file-cache-directory))
                        (make-temp-file "rg-icons-" nil ".png")))))
      (call-process
       (concat rg/icons-imagemagick-directory "convert")
       nil nil nil
       (concat "ico:" frame) "-resize" (rg/icons--size-format-string icon-size)
       ;;"-opaque" "none"
       (concat "png:" png-file))
      (if (and (file-exists-p png-file) (> (nth 7 (file-attributes png-file)) 0))
          (progn
            (rg/icons--log (format "Created png file: %s" png-file))
            (create-image (f-read-bytes png-file) 'png t :ascent 'center :mask 'heuristic))
        (progn
          (rg/icons--log (format "Could not create png file from %s" frame) 'error)
          nil)))))

(defun rg/icons--windows-create-image-for-extension (ext &optional icon-size cache-key)
  "Returns an image of the icon associated with the specified
file extension and for the given icon size. EXT should start with
a dot. When cache-key is non-nil, stores the png image under name
CACHE-KEY.png in `rg/icons-file-cache-directory'."
  (let* ((icon-size     (rg/icons-get-icon-size icon-size))
         (progid        (rg/icons--windows-reg-get-progid-for-extension ext))
         (icon-resource (when progid
                          (rg/icons--windows-reg-get-icon-resource-for-progid progid)))
         (icon-file     (when icon-resource
                          (rg/icons--windows-extract-icon-file icon-resource)))
         (image         (when icon-file
                          (rg/icons--create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg/icons--windows-create-image-for-directory (&optional icon-size cache-key)
  "Returns an image of the directory icon. When cache-key is
non-nil, stores the png image under name CACHE-KEY.png in
`rg/icons-file-cache-directory'."
  (let* ((icon-resource "%windir%/system32/shell32.dll,4")
         (icon-file (rg/icons--windows-extract-icon-file icon-resource))
         (image (when icon-file
                  (rg/icons--create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg/icons--windows-create-image-for-default-file (&optional icon-size cache-key)
  "Returns an image of the default file icon. When cache-key is
non-nil, stores the png image under name CACHE-KEY.png in
`rg/icons-file-cache-directory'."
  (let* ((icon-resource "%windir%/system32/shell32.dll,1")
         (icon-file (rg/icons--windows-extract-icon-file icon-resource))
         (image (when icon-file
                  (rg/icons--create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg/icons--windows-create-image-for-executable-file (&optional icon-size cache-key)
  "Returns an image of the default file icon for
executables. When cache-key is non-nil, stores the png image
under name CACHE-KEY.png in `rg/icons-file-cache-directory'."
  (let* ((icon-resource "%SystemRoot%/System32/shell32.dll,3")
         (icon-file (rg/icons--windows-extract-icon-file icon-resource))
         (image (when icon-file
                  (rg/icons--create-image-from-ico-file icon-file icon-size cache-key))))
    image))


;; -----------------------------------------------------------------------------
;; Caching
;; -----------------------------------------------------------------------------

(defun rg/icons--cache-key (base-key icon-size)
  (let ((icon-size (rg/icons-get-icon-size icon-size)))
    (concat base-key "-" (int-to-string icon-size))))

(defun rg/icons--retrieve-image-from-cache (cache-key)
  "Retrieves an image from the in-memory or the on-disk cache, in
that order. Returns nil if not cached."
  (when cache-key
    (let ((entry (gethash cache-key rg/icons-cache)))
      (cond 
       ((eq entry 'default) (rg/icons-create-image-for-default-file))
       (entry entry)
       (t (let ((cached-png-file
                 (concat rg/icons-file-cache-directory cache-key ".png")))
            (when (file-exists-p cached-png-file)
              (rg/icons--log (format "Reusing png file: %s" cached-png-file))
              (rg/icons--store-image-in-cache
               cache-key
               (create-image (f-read-bytes cached-png-file) 'png t
                             :ascent 'center :mask 'heuristic)))))))))

(defun rg/icons--store-image-in-cache (cache-key image)
  (when image
    (rg/icons--log (format "Caching image for key: %s" cache-key))
    (puthash cache-key image rg/icons-cache))
  image)

(defun rg/icons-clear-memory-cache ()
  "Clear the in-memory icon cache."
  (interactive)
  (clrhash rg/icons-cache))

(defun rg/icons-clear-file-cache ()
  "Clear the on-disk file cache."
  (interactive)
  (delete-directory rg/icons-file-cache-directory t t)
  (make-directory rg/icons-file-cache-directory t))

(defun rg/icons-clear-cache ()
  "Clear the in-memory and file cache."
  (interactive)
  (rg/icons-clear-file-cache)
  (rg/icons-clear-memory-cache))


(defun rg/icons--save-memory-cache ()
  (when (and rg/icons-persistent-cache-file
             (file-writable-p rg/icons-persistent-cache-file))
    (rg/icons--log (format "Saving in-memory cache to: %s" rg/icons-persistent-cache-file))
    (with-temp-file rg/icons-persistent-cache-file
      (set-buffer-file-coding-system 'binary t)
      (insert (let (print-length) (prin1-to-string rg/icons-cache))))))

(defun rg/icons--read-memory-cache ()
  (when rg/icons-persistent-cache-file
    (if (file-exists-p rg/icons-persistent-cache-file)
        (with-demoted-errors
            "rg-icons: Error: Failed to restore in-memory cache from: %S"
          (with-temp-buffer
              (insert-file-contents-literally rg/icons-persistent-cache-file)
              (setq rg/icons-cache (read (buffer-string))))
          (rg/icons--log (format "In-memory-cache restored from file: %s"
                                 rg/icons-persistent-cache-file)))
      (rg/icons--log
       (format "File for restoring in memory-cache not found: %s"
               rg/icons-persistent-cache-file) 'error)))
  rg/icons-cache)

;; restore the cache from last time (does not work right now; disabled)
(unless (rg/icons--read-memory-cache)
  (setq rg/icons-cache (make-hash-table :test 'equal)))

;; auto-save on exit
(add-hook 'kill-emacs-hook 'rg/icons--save-memory-cache)

;; -----------------------------------------------------------------------------
;; Main entry points (OS-independent)
;; -----------------------------------------------------------------------------

(defun rg/icons-create-image-for-directory (&optional icon-size)
  "Returns an image of the directory icon."
  (let* ((cache-key (rg/icons--cache-key "directory" icon-size))
         (cached-image (rg/icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg/icons--store-image-in-cache
       cache-key
       (rg/icons--windows-create-image-for-directory icon-size cache-key)))))

(defun rg/icons-create-image-for-default-file (&optional icon-size)
  "Returns an image of the default file icon."
  (let* ((cache-key (rg/icons--cache-key "default" icon-size))
         (cached-image (rg/icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg/icons--store-image-in-cache
       cache-key
       (rg/icons--windows-create-image-for-default-file icon-size cache-key)))))

(defun rg/icons-create-image-for-executable-file (&optional icon-size)
  "Returns an image of an executable file."
  (let* ((cache-key (rg/icons--cache-key ".exe" icon-size))
         (cached-image (rg/icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg/icons--store-image-in-cache
       cache-key
       (rg/icons--windows-create-image-for-executable-file icon-size cache-key)))))

(defun rg/icons-create-image-for-extension (ext &optional icon-size)
  "Returns an image of the icon associated with the specified
file extension and for the given icon size. EXT should start with
a dot."
  (let* ((cache-key (rg/icons--cache-key ext icon-size))
         (cached-image (rg/icons--retrieve-image-from-cache cache-key)))
    (if cached-image
        cached-image
      (rg/icons--store-image-in-cache
       cache-key
       (rg/icons--windows-create-image-for-extension ext icon-size cache-key)))))

(defun rg/icons-create-image-for-file (file &optional icon-size)
  "Returns an image of the icon associated with the given
  FILE. FILE can be a directory or it can be non-existing. When
  no icon is found, returns the default file icon."
  (let* ((symlink (file-symlink-p file))
         (ext (if symlink
                  (file-name-extension symlink t)
                (file-name-extension file t)))
         (image))
    (cond
     ((file-directory-p file)
      (setq image (rg/icons-create-image-for-directory icon-size)))
     ((and ext (equal ext ".exe"))
      (setq image (rg/icons-create-image-for-executable-file icon-size)))
     ((and ext (not (equal ext "")))
      (setq image (rg/icons-create-image-for-extension ext icon-size))))
    (unless image
      (setq image (rg/icons-create-image-for-default-file icon-size))
      ;; store also misses with default icon to not try again (only in memory)
      (when (and ext (not (equal ext "")))
        (rg/icons--store-image-in-cache (rg/icons--cache-key ext icon-size) 'default)))
    image))

(provide 'rg-icons)
