(defvar exttextcat-build-dir (locate-user-emacs-file "exttextcat/")
  "libexttextcat library wrapper executable directory.")

(defvar exttextcat-executable (concat exttextcat-build-dir "exttextcat")
  "Path to libexttextcat library wrapper executable.")

(defvar exttextcat-package-dir
  (let ((script-file (or load-file-name
                         buffer-file-name))) ; For direct evaluation
    (file-name-directory script-file))
  "This package directory.")

(defvar exttextcat-language-models
  '(("/usr/share/libexttextcat/en.lm" . "english")
    ("/usr/share/libexttextcat/pl.lm" . "polish"))
  "List of pairs of language model and corresponding language name.

Language name is used as ispell dictionary name.")

(defvar exttextcat--fpdb-config-file nil
  "Path to temporarily generated config file of language models.")

;;;###autoload
(defun exttextcat-guess-language-buffer ()
  "Guess language of current buffer."
  (interactive)
  (let ((language (exttextcat-get-language-for-buffer)))
    (if language
        (progn
          (message "Setting language buffer to %s" language)
          (ispell-change-dictionary language))
      (message "Buffer language could not be determined"))))

(defun exttextcat-get-language-for-buffer ()
  "Determine language of current buffer.

If the language could not be determined returns nil."
  (let* ((exit-status-output (exttextcat-call-c-wrapper))
         (exit-status (car exit-status-output))
         (output (cdr exit-status-output)))
    (if (zerop exit-status)
        (exttextcat-find-matching-language (exttextcat-extract-language output))
      (message "Program exited with %d exit code and output: %s"
               exit-status
               output))))

(defun exttextcat-find-matching-language (language)
  (let ((known-languages (mapcar 'cdr exttextcat-language-models)))
    (if (member language known-languages)
        language
      nil)))

(defun exttextcat-extract-language (lang-output)
  "Extract language of library wrapper output.

Returns nil if language could not be determined."
  (let ((maybe-language (substring lang-output 1 (- (length lang-output) 2))))
    (unless (string-match-p "\\]\\[" maybe-language)
      maybe-language)))

(defun exttextcat-call-c-wrapper ()
  (let ((call-wrapper-fn
         (apply 'apply-partially
                (if (and buffer-file-name (file-readable-p buffer-file-name))
                    (list 'exttextcat-call-c-wrapper-on-file buffer-file-name)
                  (list 'exttextcat-call-c-wrapper-on-buffer (current-buffer))))))
    (exttextcat-call-c-wrapper-with-fn call-wrapper-fn)))

(defun exttextcat-call-c-wrapper-with-fn (call-process-fn)
  "Call CALL-PROCESS-FN on `exttextcat-executable' fpdb config file and output buffer.

The function is expected to return exit status and process output
in given buffer."
  (unless (file-executable-p exttextcat-executable)
    (error "Call `exttextcat-install-wrapper' command to build library wrapper"))
  (with-temp-buffer
    (let ((exit-status (funcall call-process-fn
                                exttextcat-executable
                                (exttextcat-get-fpdb-config-file)
                                (current-buffer)))
          (cmd-output (buffer-string)))
      (cons exit-status cmd-output))))

(defun exttextcat-call-c-wrapper-on-file (input-file executable config-file output-buffer)
  "Call process on INPUT-FILE."
  (call-process executable
                nil
                output-buffer
                nil
                config-file
                input-file))

(defun exttextcat-call-c-wrapper-on-buffer (input-buffer executable config-file output-buffer)
  "Call process on INPUT-BUFFER."
  (with-current-buffer input-buffer
    (call-process-region (point-min)
                         (point-max)
                         executable
                         nil
                         output-buffer
                         nil
                         config-file)))

(defun exttextcat-get-fpdb-config-file ()
  "Get and creates if needed library config file."
  (unless (and exttextcat--fpdb-config-file (file-exists-p exttextcat--fpdb-config-file))
    (setq exttextcat--fpdb-config-file (make-temp-file "fpdb.conf"))
    (with-temp-file exttextcat--fpdb-config-file
      (insert (exttextcat-build-fpdb-config))))
  exttextcat--fpdb-config-file)

(defun exttextcat-build-fpdb-config ()
  "Build library config based on `exttextcat-language-models'."
  (mapconcat (lambda (model-language)
               (concat (car model-language) " " (cdr model-language)))
             exttextcat-language-models
             "\n"))

;;;###autoload
(defun exttextcat-install-wrapper ()
  "Build and install library wrapper to `exttextcat-build-dir'."
  (interactive)
  (let ((default-directory exttextcat-package-dir)
        (build-cmd (concat "make DESTDIR=" exttextcat-build-dir)))
    (make-directory exttextcat-build-dir t)
    (compilation-start build-cmd nil (lambda (mode-name)
                                       "*exttextcat-compilation*"))))

(provide 'exttextcat)
