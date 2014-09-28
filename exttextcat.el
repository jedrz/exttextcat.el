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
    (unless (zerop exit-status)
      (error "Program exited with %d exit code" exit-status))
    (exttextcat-extract-lang output)))

(defun exttextcat-extract-lang (lang-output)
  "Extract language of library wrapper output.

Returns nil if language could not be determined."
  (let ((maybe-language (substring lang-output 1 (- (length lang-output) 2))))
    (unless (string-match-p "\\]\\[" maybe-language)
      maybe-language)))

(defun exttextcat-call-c-wrapper (&optional input-file)
  "Call library wrapper on INPUT-FILE or current buffer file name.

Cons cell of exit status and output is returned."
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
