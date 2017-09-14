(require 'header2)

(define-advice make-header (:after (&rest args) add-lexbind-variable)
  "Add `lexical-binding: t' to header."
  (when (eq major-mode 'emacs-lisp-mode)
    (save-excursion
      (add-file-local-variable-prop-line 'lexical-binding t))))

(defsubst header-not-part-of-emacs ()
  "Insert line declaring that this file is not part of Emacs."
  (when (eq major-mode 'emacs-lisp-mode)
    (insert header-prefix-string "This file is NOT part of GNU Emacs.\n")))

(defsubst header-completely-blank ()
  "Insert an empty line to file header (not even `header-prefix-string')."
  (insert "\n"))

(setq header-copyright-notice
      (format-time-string "Copyright (C) %Y Daniel Barreto\n"))

;; Set up headers when creating an elisp file
(add-hook 'emacs-lisp-mode-hook #'auto-make-header)

;; Update headers on save
(add-hook 'write-file-hooks #'auto-update-file-header)

;; Override `header-eof' to not insert a separator line
(defun header-eof ()
  "Insert comment indicating end of file."
  (goto-char (point-max))
  (insert "\n")
  (insert comment-start
          (concat (and (= 1 (length comment-start)) header-prefix-string)
                  (if (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-name))
                  " ends here"
                  (or (nonempty-comment-end) "\n"))))

;; Function to insert `provide' statement at end of file; This is
;; used in `make-header-hook'.
(defun header-provide-statement ()
  "Insert `provide' statement."
  (save-excursion
    (goto-char (point-max))
    (insert
     (format "\n%s"
             (pp-to-string `(provide ',(intern (f-base (buffer-file-name)))))))))

;; Cleanup whitespaces from header
(defun vlt--cleanup-elisp-header (orig-fun &rest args)
  (apply orig-fun args)
  (whitespace-cleanup))
(advice-add 'auto-make-header :around #'vlt--cleanup-elisp-header)

;; Prevent `auto-make-header' from setting the buffer modified flag
(define-advice auto-make-header
    (:around (orig-fun &rest args) dont-set-buffer-modified)
  "Don't set the buffer modified flag."
  (apply orig-fun args)
  (set-buffer-modified-p nil))

(provide 'vlt-config-header2)
