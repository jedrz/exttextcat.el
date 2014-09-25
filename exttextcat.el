(defvar exttextcat-executable (exttextcat-path-for-file-in-project "exttextcat"))

(defvar exttextcat-fpdb-config (exttextcat-path-for-file-in-project "fpdb.conf"))

;;;###autoload
(defun exttextcat-guess-language-buffer ()
  (let ((language (exttextcat-get-language-for-buffer)))
    (message "Setting language buffer to %s" language)
    (ispell-change-dictionary language)))

(defun exttextcat-get-language-for-buffer ()
  (let* ((exit-status-output (exttextcat-call-c-wrapper))
         (exit-status (car exit-status-output))
         (output (cdr exit-status-output)))
    (unless (zerop exit-status)
      (error "Program exited with %d exit code" exit-status))
    (exttextcat-extract-lang output)))

(defun exttextcat-extract-lang (lang-output)
  (substring lang-output 1 (- (length lang-output) 2)))

(defun exttextcat-call-c-wrapper (&optional input-file)
  (setq input-file (or input-file buffer-file-name))
  (unless input-file
    (error "Buffer is not visiting file"))
  (with-temp-buffer
    (let ((exit-status (call-process exttextcat-executable
                                     nil
                                     (current-buffer)
                                     nil
                                     exttextcat-fpdb-config
                                     input-file))
          (cmd-output (buffer-string)))
      (cons exit-status cmd-output))))

(defun exttextcat-path-for-file-in-project (filename)
  (let* ((script-file (or load-file-name buffer-file-name))
         (script-directory (file-name-directory script-file)))
    (concat script-directory filename)))

(provide 'exttextcat)
