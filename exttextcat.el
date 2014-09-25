(defvar exttextcat-executable (exttextcat-path-for-file-in-project "exttextcat"))

(defvar exttextcat-language-models
  '(("/usr/share/libexttextcat/en.lm" . "english")
    ("/usr/share/libexttextcat/pl.lm" . "polish")))

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
                                     (exttextcat-build-fpdb-config-file)
                                     input-file))
          (cmd-output (buffer-string)))
      (cons exit-status cmd-output))))

(defun exttextcat-build-fpdb-config-file ()
  (let ((temp-file (make-temp-file "fpdb.conf")))
    (with-temp-file temp-file
      (insert (exttextcat-build-fpdb-config)))
    temp-file))

(defun exttextcat-build-fpdb-config ()
  (mapconcat (lambda (model-language)
               (concat (car model-language) " " (cdr model-language)))
             exttextcat-language-models
             "\n"))

(defun exttextcat-path-for-file-in-project (filename)
  (let* ((script-file (or load-file-name buffer-file-name))
         (script-directory (file-name-directory script-file)))
    (concat script-directory filename)))

(provide 'exttextcat)
