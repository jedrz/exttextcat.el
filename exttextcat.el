(defvar exttextcat-executable (concat exttextcat-build-dir "exttextcat"))

(defvar exttextcat-build-dir (locate-user-emacs-file "exttextcat/"))

(defvar exttextcat-package-dir
  (let ((script-file (or load-file-name
                         buffer-file-name))) ; For direct evaluation
    (file-name-directory script-file)))

(defvar exttextcat-language-models
  '(("/usr/share/libexttextcat/en.lm" . "english")
    ("/usr/share/libexttextcat/pl.lm" . "polish")))

(defvar exttextcat--fpdb-config-file nil)

;;;###autoload
(defun exttextcat-guess-language-buffer ()
  (interactive)
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
  (unless (file-executable-p exttextcat-executable)
    (error "Call `exttextcat-install-wrapper' command to build library wrapper"))
  (with-temp-buffer
    (let ((exit-status (call-process exttextcat-executable
                                     nil
                                     (current-buffer)
                                     nil
                                     (exttextcat-get-fpdb-config-file)
                                     input-file))
          (cmd-output (buffer-string)))
      (cons exit-status cmd-output))))

(defun exttextcat-get-fpdb-config-file ()
  (unless (and exttextcat--fpdb-config-file (file-exists-p exttextcat--fpdb-config-file))
    (setq exttextcat--fpdb-config-file (make-temp-file "fpdb.conf"))
    (with-temp-file exttextcat--fpdb-config-file
      (insert (exttextcat-build-fpdb-config))))
  exttextcat--fpdb-config-file)

(defun exttextcat-build-fpdb-config ()
  (mapconcat (lambda (model-language)
               (concat (car model-language) " " (cdr model-language)))
             exttextcat-language-models
             "\n"))

;;;###autoload
(defun exttextcat-install-wrapper ()
  (interactive)
  (let ((default-directory exttextcat-package-dir)
        (build-cmd (concat "make DESTDIR=" exttextcat-build-dir)))
    (make-directory exttextcat-build-dir t)
    (compilation-start build-cmd nil (lambda (mode-name)
                                       "*exttextcat-compilation*"))))

(provide 'exttextcat)
