;;; vlt-config-emacs-lisp.el ---
;;
;; Copyright © 2011-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((vlt-config-lisp "1.0.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Copy of `prelude-emacs-lisp.el' configuration, with some small
;; adaptations.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'vlt-config-lisp)
(require 'crux)

(vlt-require-packages '(elisp-slime-nav))

(defun vlt-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p user-emacs-directory (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun vlt-config-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'vlt-config-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
;; (define-key emacs-lisp-mode-map (kbd "C-c e") 'macroexpand-point)  ;; TODO -- copy from your local defuns

(defun vlt-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun vlt-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'vlt-lisp-coding-hook)
  (eldoc-mode t)
  (vlt-recompile-elc-on-save)
  (setq mode-name "EL")
  (vlt-conditional-emacs-lisp-checker))

(setq vlt-emacs-lisp-mode-hook 'vlt-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'vlt-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; ielm is an interactive Emacs Lisp shell
(defun vlt-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'vlt-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(setq vlt-ielm-mode-hook 'vlt-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'vlt-ielm-mode-hook)))

(eval-after-load "elisp-slime-nav"
  '(diminish 'elisp-slime-nav-mode))
(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(eval-after-load "ielm"
  '(progn
     (define-key ielm-map (kbd "M-(") (vlt-wrap-with "("))
     (define-key ielm-map (kbd "M-\"") (vlt-wrap-with "\""))))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(provide 'vlt-config-emacs-lisp)

;;; vlt-config-emacs-lisp.el ends here
