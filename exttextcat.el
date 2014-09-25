(defvar exttextcat-executable (exttextcat-path-for-file-in-project "exttextcat"))

(defvar exttextcat-fpdb-config (exttextcat-path-for-file-in-project "fpdb.conf"))

;;;###autoload
(defun exttextcat-guess-language-buffer ()
  (let ((language (exttextcat-get-language-for-buffer)))
    (message "Setting language buffer to %s" language)
    (ispell-change-dictionary language)))

(defun exttextcat-get-language-for-buffer ()
  (let ((lang-output (exttextcat-call-c-wrapper)))
    (exttextcat-extract-lang lang-output)))

(defun exttextcat-extract-lang (lang-output)
  (substring lang-output 1 (- (length lang-output) 2)))

(defun exttextcat-call-c-wrapper ()
  (shell-command-to-string
   (format "%s %s %s"
           exttextcat-executable
           exttextcat-fpdb-config
           buffer-file-name)))

(defun exttextcat-path-for-file-in-project (filename)
  (let* ((script-file (or load-file-name buffer-file-name))
         (script-directory (file-name-directory script-file)))
    (concat script-directory filename)))

(provide 'exttextcat)
