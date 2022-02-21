;;; vlt-lang-elisp.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-lang-elisp.el
;; Author:
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sat Feb 19 14:44:16 2022 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package eval-sexp-fu)


(use-package header2
  :config
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


  (setq make-header-hook
        '(header-title
          header-blank
          header-file-name
          header-author
          header-copyright
          header-creation-date
          header-blank
          header-lib-requires
          header-end-line
          header-commentary
          header-blank
          header-blank
          header-blank
          header-end-line
          header-free-software
          header-code
          header-eof))

  ;; Set up headers when creating an elisp file
  (add-hook 'emacs-lisp-mode-hook #'auto-make-header)

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

  ;; Cleanup whitespaces from header
  (defun vlt/cleanup-elisp-header (orig-fun &rest args)
    (apply orig-fun args)
    (whitespace-cleanup))
  (advice-add 'auto-make-header :around #'vlt/cleanup-elisp-header)

  ;; Prevent `auto-make-header' from setting the buffer modified flag
  (define-advice auto-make-header
      (:around (orig-fun &rest args) dont-set-buffer-modified)
    "Don't set the buffer modified flag."
    (apply orig-fun args)
    (set-buffer-modified-p nil)))


(use-package pp+
  :bind (([remap eval-expression] . pp-eval-expression)
         :map emacs-lisp-mode-map
         ("C-c C-p" . pp-eval-last-sexp)
         ("C-c C-k" . pp-eval-buffer)))


(provide 'vlt-lang-elisp)

;;; vlt-lang-elisp.el ends here
