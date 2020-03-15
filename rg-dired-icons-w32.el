;;; rg-dired-icons-w32.el -*- lexical-binding: t; -*-
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
;; Supports native Windows Emacs and Emacs run in WSL. In WSL, use Linux format
;; for paths and filenames throughout, but make sure that
;; rg-dired-icons-file-cache-directory and rg-dired-icons-persistent-cache-file
;; are also accessible from Windows.

;;; Code:
(require 'f)

;; -----------------------------------------------------------------------------
;; OS-specific variables
;; -----------------------------------------------------------------------------

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
;; WSL glue
;; -----------------------------------------------------------------------------

(defvar rg-dired-icons-w32-cmd-file-name
  (if (string-match-p "cmdproxy" shell-file-name)
      shell-file-name
    "cmd.exe")
  "Set to cmdproxy.exe in Windows or cmd.exe in WSL.")

(defun rg-dired-icons-w32--expand-path (path)
  "Expands variables in path using cmd.exe."
  (let ((default-directory (if (file-directory-p "/mnt/c/")
                               ;; make sure that cmd.exe starts in a win dir
                               "/mnt/c"
                             default-directory)))
    (string-trim-right
     (shell-command-to-string
      (concat "cmd.exe /C echo "
              (replace-regexp-in-string "\\\\" "\\\\\\\\" path))))))

(defun rg-dired-icons-w32--to-native-path (winpath)
  "Converts Windows path to native path (only changes path when in WSL)."
  (if (eq system-type 'windows-nt)
      winpath
    (string-trim-right
     (shell-command-to-string (concat "wslpath \"" winpath "\"")))))

(defun rg-dired-icons-w32--to-win-path (native-path)
  "Converts native path to Windows path (only changes path when in WSL)."
  (if (eq system-type 'windows-nt)
      native-path
    (let ((part0 native-path)
          part1 windows-path)
      (while part0
        (if (file-exists-p part0)
            (progn
              (setq windows-path
                    (concat (string-trim-right
                             (shell-command-to-string
                              (concat "wslpath -w \"" part0 "\"")))
                            (when part1 "\\") part1))
              (setq part0 nil))
          (setq part1
                (concat
                 (file-name-nondirectory (string-trim-right part0 "[/]+"))
                 (when part1 "\\") part1))
          (setq part0 (file-name-directory (string-trim-right part0 "[/]+")))))
      windows-path)))

;; -----------------------------------------------------------------------------
;; Registry querying
;; -----------------------------------------------------------------------------

(defun rg-dired-icons--w32-reg-query (key-arg value-arg)
  "Query the Windows registry.
KEY-ARG and VALUE-ARG are used as arguments to \"reg query\".
Returns nil on error."
  (let ((result (shell-command-to-string (format "reg.exe query %s %s" key-arg value-arg))))
    (when (and result (not (string-match-p "\\`ERROR" result)))
      (replace-regexp-in-string "" "" result))))

(defun rg-dired-icons--w32-reg-entry-first-key (entry)
  "Extract the first value of type REG_SZ or REG_EXPAND_SZ from ENTRY.
ENTRY should be a result of
`rg-dired-icons--w32-reg-query'.  Returns nil on error."
  (save-match-data
    (when (and entry (string-match "^ * \\([^ ]+\\) *REG_" entry))
      (match-string 1 entry))))

(defun rg-dired-icons--w32-reg-entry-first-value (entry)
  "Extract the first value of type REG_SZ or REG_EXPAND_SZ from ENTRY.
ENTRY should be a result of
`rg-dired-icons--w32-reg-query'.  Returns nil on error."
  (let ((result
         (save-match-data
           (when (and entry (string-match "^.* +REG_\\(EXPAND_\\)?SZ +\\([^ ].*\\)$" entry))
             (match-string 2 entry)))))
    (when (and result (not (string-match-p "VALUE NOT SET" (upcase result))))
      result)))

(defun rg-dired-icons--w32-reg-entry-query-first-value (key-arg value-arg)
  "Query the Windows registry and extract the first value.
KEY-ARG and VALUE-ARG are used as arguments to \"reg query\".
Returns nil on error."
  (let ((entry (rg-dired-icons--w32-reg-query key-arg value-arg)))
    (when entry
      (rg-dired-icons--w32-reg-entry-first-value entry))))

(defun rg-dired-icons--w32-reg-entry-query-first-key (key-arg value-arg)
  "Query the Windows registry and extract the first key.
KEY-ARG and VALUE-ARG are used as arguments to \"reg query\".
Returns nil on error."
  (let ((entry (rg-dired-icons--w32-reg-query key-arg value-arg)))
    (when entry
      (rg-dired-icons--w32-reg-entry-first-key entry))))

(defun rg-dired-icons--w32-reg-get-progid-for-extension (ext)
  "Return the progid registered for extension EXT or nil when none."
  (let* ((progid (or (rg-dired-icons--w32-reg-entry-query-first-value
                      (format "\"HKCR\\%s\\Userchoice\"" ext) "/v ProgId")
                     (rg-dired-icons--w32-reg-entry-query-first-value
                      (format "\"HKCR\\%s\"" ext) "/ve")
                     (rg-dired-icons--w32-reg-entry-query-first-value
                      (format "\"HKCR\\%s\\FriendlyTypeName\"" ext) "/ve")
                     (rg-dired-icons--w32-reg-entry-query-first-value
                      (format "\"HKCR\\%s\\OpenWithProgids\"" ext) "/ve")
                     (rg-dired-icons--w32-reg-entry-query-first-key
                      (format "\"HKCR\\%s\\OpenWithProgids\"" ext) ""))))
    (if progid
        (rg-dired-icons--log (format "Found progid for extension %s: %s" ext progid))
      (rg-dired-icons--log (format "Did not find progid for extension %s" ext) 'error))
    progid
    ))

(defun rg-dired-icons--w32-reg-get-icon-resource-for-progid (progid)
  "Return the icon resource (of format \"filename,icon-number\") registered with PROGID or nil when not present."
  (let* ((entry (rg-dired-icons--w32-reg-query (format "\"HKCR\\%s\\DefaultIcon\"" progid) "/ve"))
         (command (unless entry (rg-dired-icons--w32-reg-entry-first-value
                                 (rg-dired-icons--w32-reg-query
                                  (format "\"HKCR\\%s\\Shell\\Open\\Command\"" progid)
                                  "/ve"))))
         (icon-resource (cond
                         (entry
                          (rg-dired-icons--w32-reg-entry-first-value entry))
                         ((and command (string-match "^\"\\([^\"]+\\)\"" command))
                          (concat (match-string 1 command) ",0"))
                         ((and command (string-match "^\\([^ ]+\\)" command))
                          (concat (match-string 1 command) ",0")))))
    (if icon-resource
        (rg-dired-icons--log
         (format "Found icon resource for progid %s: %s" progid icon-resource))
      (rg-dired-icons--log (format "Did not find icon resource for progid %s" progid) 'error))
    icon-resource))


;; -----------------------------------------------------------------------------
;; Icon extraction
;; -----------------------------------------------------------------------------

(defun rg-dired-icons--w32-extract-icon-file (icon-resource)
  "Extracts the ico file from ICON-RESOURCE.
Returns nil on error."
  (let* ((file-n (split-string icon-resource ","))
         (file (rg-dired-icons--unquote (nth 0 file-n)))
         (file (rg-dired-icons-w32--expand-path file))
         (file (if (and (not (file-exists-p (rg-dired-icons-w32--to-native-path file)) )
                        (not (string-match-p "[/|\\]" file)))
                   (rg-dired-icons-w32--expand-path (concat "%WinDir%\\System32\\" file))
                 file))
         (alt-file (concat
                    (replace-regexp-in-string
                     "[Ss][Yy][Ss][Tt][Ee][Mm][0-9][0-9]" "SystemResources"
                     file)
                    ".mun"))
         (file (if (file-exists-p (rg-dired-icons-w32--to-native-path alt-file))
                   alt-file
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
        (make-directory ico-dir)
        (with-temp-file (concat ico-dir "CONTENTS")
          (insert file))

        ;; try to extract icons
        (let ((default-directory (file-truename rg-dired-icons-w32-resource-hacker-directory)))
          (call-process
           (concat default-directory "ResourceHacker.exe")
           nil nil nil
           "-open" (replace-regexp-in-string "\\\\" "/" file)
           "-save" (replace-regexp-in-string "\\\\" "/" (rg-dired-icons-w32--to-win-path rc-file))
           "-action" "extract"
           "-mask" "ICONGROUP,,"))
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
       ;; if file with requested number exists, use it
       ((file-exists-p (concat ico-dir n ".ico"))
        (setq extracted-ico-file (concat ico-dir n ".ico")))
       ;; if file is an ico file, use it
       ((equal (file-name-extension file t) ".ico")
        (progn
          (copy-file (rg-dired-icons-w32--to-native-path file) ico-file t)
          (setq extracted-ico-file ico-file)
          (rg-dired-icons--log (format "Copied icon %s: %s" file ico-dir))))
       (t
        ;; else treat n as an index and see if there is an icon file with this
        ;; index
        (let ((icons (directory-files ico-dir nil "[0-9]+\\.ico")))
          (if (> (length icons) (string-to-number n))
              (setq extracted-ico-file
                    (concat ico-dir (nth (string-to-number n) icons)))
            ;; otherwise, if tehre is just one icon, use it
            (let ((icons (directory-files ico-dir nil ".+\\.ico")))
              (if (= (length icons) 1)
                  (setq extracted-ico-file
                        (concat ico-dir (nth 0 icons)))
                ;; otherwise, there is an error
                (rg-dired-icons--log
                 (format "Could not extract icons from file %s" file) 'error)))))))

      (if extracted-ico-file
          (rg-dired-icons--log
           (format "Using icon from icon resource %s: %s" icon-resource extracted-ico-file))
        (rg-dired-icons--log
         (format "Could not determine icon in icon resource %s" icon-resource)
         'error))
      extracted-ico-file)))

(defun rg-dired-icons--w32-get-icon-frame (ico-file &optional icon-size)
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

(defun rg-dired-icons--w32-size-format-string (&optional icon-size)
  "Format string to use with ImageMagick convert.
Uses the specified ICON-SIZE."
  (let ((icon-size (if icon-size icon-size rg-dired-icons-default-icon-size)))
    (concat (int-to-string icon-size) "x" (int-to-string icon-size))))

(defun rg-dired-icons--w32-create-image-from-ico-file (ico-file &optional icon-size cache-key)
  "Create an image from ICO-FILE.
Return the image of the frame that best matches the specified
ICON-SIZE in the given ico-file.  When CACHE-KEY is non-nil,
stores the png image under name CACHE-KEY.png in
`rg-dired-icons-file-cache-directory'.  Returns nil on error."
  (when (file-exists-p ico-file)
    (let ((icon-size  (rg-dired-icons-get-icon-size icon-size))
          (frame (rg-dired-icons--w32-get-icon-frame ico-file icon-size))
          (png-file (if cache-key
                        (concat rg-dired-icons-file-cache-directory cache-key ".png")
                      (let ((temporary-file-directory rg-dired-icons-file-cache-directory))
                        (make-temp-file "rg-dired-icons-" nil ".png")))))
      (call-process
       (concat rg-dired-icons-w32-imagemagick-directory "convert")
       nil nil nil
       (concat "ico:" frame) "-resize" (rg-dired-icons--w32-size-format-string icon-size)
       "-depth" "32" ;; required to handle transparency well in Emacs
       (concat "png:" png-file))
      (if (and (file-exists-p png-file) (> (nth 7 (file-attributes png-file)) 0))
          (progn
            (rg-dired-icons--log (format "Created png file: %s" png-file))
            (create-image (f-read-bytes png-file) 'png t :ascent 'center :mask 'heuristic :scale 1))
        (progn
          (rg-dired-icons--log (format "Could not create png file from %s" frame) 'error)
          nil)))))


;; -----------------------------------------------------------------------------
;; Main entry points (OS-dependent)
;; -----------------------------------------------------------------------------

(defun rg-dired-icons--ensure-external-programs ()
  "Check whether all required external programs are present."
  (unless (executable-find "reg.exe")
    (error "rg-dired-icons: reg executable not found. Are you on Windws?"))
  (unless (executable-find (concat rg-dired-icons-w32-imagemagick-directory "convert"))
    (error "rg-dired-icons: ImageMagick's convert exectutable not found. Is rg-dired-icons-w32-imagemagick-directory set correctly?."))
  (unless (executable-find (concat rg-dired-icons-w32-imagemagick-directory "identify"))
    (error "rg-dired-icons: ImageMagick's identify exectutable not found. Is rg-dired-icons-w32-imagemagick-directory set correctly?."))
  (unless (executable-find (concat rg-dired-icons-w32-resource-hacker-directory "ResourceHacker.exe"))
    (error "rg-dired-icons: ResourceHacker executable not found. Is rg-dired-icons-w32-resource-hacker-directory set correctly?.")))


(defun rg-dired-icons--create-image-for-directory (&optional icon-size cache-key)
  "Create an image of the directory icon with the specified ICON-SIZE.
When CACHE-KEY is non-nil, stores the png image under name
CACHE-KEY.png in `rg-dired-icons-file-cache-directory'."
  (let* ((icon-resource "%windir%\\SystemResources\\imageres.dll.mun,4")
         (icon-file (rg-dired-icons--w32-extract-icon-file icon-resource))
         (image (when icon-file
                  (rg-dired-icons--w32-create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg-dired-icons--create-image-for-executable-file (&optional icon-size cache-key)
  "Create an image of the default icon for executables.
Uses the specified ICON-SIZE.  When CACHE-KEY is non-nil, stores
the png image under name CACHE-KEY.png in
`rg-dired-icons-file-cache-directory'."
  (let* ((icon-resource "%windir%\\SystemResources\\imageres.dll.mun,15")
         (icon-file (rg-dired-icons--w32-extract-icon-file icon-resource))
         (image (when icon-file
                  (rg-dired-icons--w32-create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg-dired-icons--create-image-for-extension (ext &optional icon-size cache-key)
  "Create an image for the icon associated with extension EXT.
Return the image of size ICON-SIZE.  EXT should start with a
dot.  When CACHE-KEY is non-nil, stores the png image under name
CACHE-KEY.png in `rg-dired-icons-file-cache-directory'."
  (let* ((icon-size     (rg-dired-icons-get-icon-size icon-size))
         (progid        (rg-dired-icons--w32-reg-get-progid-for-extension ext))
         (icon-resource (when progid
                          (rg-dired-icons--w32-reg-get-icon-resource-for-progid progid)))
         (icon-file     (when icon-resource
                          (rg-dired-icons--w32-extract-icon-file icon-resource)))
         (image         (when icon-file
                          (rg-dired-icons--w32-create-image-from-ico-file icon-file icon-size cache-key))))
    image))

(defun rg-dired-icons--create-image-for-default-file (&optional icon-size cache-key)
  "Create an image of the default file icon with the specified ICON-SIZE.
When CACHE-KEY is non-nil, stores the png image under name
CACHE-KEY.png in `rg-dired-icons-file-cache-directory'."
  (let* ((icon-resource "%windir%\\SystemResources\\imageres.dll.mun,2")
         (icon-file (rg-dired-icons--w32-extract-icon-file icon-resource))
         (image (when icon-file
                  (rg-dired-icons--w32-create-image-from-ico-file icon-file icon-size cache-key))))
    image))


(provide 'rg-dired-icons-w32)

;;; rg-dired-icons.el ends here
