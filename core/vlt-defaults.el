;;; vlt-defaults.el --- 
;; 
;; Filename: vlt-defaults.el
;; Description: 
;; Author: Daniel Barreto
;; Maintainer: 
;; Created: Thu Sep  7 13:44:11 2017 (+0200)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This is a mix of Phil Hagelberg's `better-defaults.el', prelude's
;; `prelude-editor.el', magnar's `sane-defaults.el' and some defaults
;; of my own.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'misc)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Welcome me into a nice scratch buffer.
(setq initial-scratch-message "
;;           )  (    (                      )
;;        ( /(  )\\ ) )\\ )   (      *   ) ( /(
;;  (   ( )\\())(()/((()/(   )\\   ` )  /( )\\())
;;  )\\  )((_)\\  /(_))/(_)|(((_)(  ( )(_)|(_)\\
;; ((_)((_)((_)(_)) (_))  )\\ _ )\\(_(_()) _((_)
;; \\ \\ / // _ \\| |  | _ \\ (_)_\\(_)_   _|| || |
;;  \\ V /| (_) | |__|   /  / _ \\   | |  | __ |
;;   \\_/  \\___/|____|_|_\\ /_/ \\_\\  |_|  |_||_|

;;           Spiral Out. Keep Going.

")

;; Window good behavior
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Buffers good behavior
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; History
(require 'saveplace)
(setq-default save-place t)

(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" vlt-backups-dir))
(savehist-mode t)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" vlt-backups-dir)
      recentf-max-saved-items 100
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

;; (defun prelude-recentf-exclude-p (file)
;;   "A predicate to decide whether to exclude FILE from recentf."
;;   (let ((file-dir (file-truename (file-name-directory file))))
;;     (-any-p (lambda (dir)
;;               (string-prefix-p dir file-dir))
;;             (mapcar 'file-truename (list prelude-savefile-dir package-user-dir)))))
;; (add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)
(recentf-mode t)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; Auto refresh buffers
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)  ; Also auto refresh dired
(setq auto-revert-verbose nil)                ; but be quiet about it

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Highlight current line
(global-hl-line-mode t)

;; Show me wrapping parens
(show-paren-mode t)

;; Don't indent with tabs
(setq-default indent-tabs-mode nil)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; Show active region
(transient-mark-mode t)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode t)

;; Always display line and column numbers
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Sentences do not need double spaces to end. Are we sure though?
;; (set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; No electric indent
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
;; (defadvice pop-to-mark-command (around ensure-new-position activate)
;;   (let ((p (point)))
;;     (when (eq last-command 'save-region-or-current-line)
;;       ad-do-it
;;       ad-do-it
;;       ad-do-it)
;;     (dotimes (i 10)
;;       (when (= p (point)) ad-do-it))))

;; (setq set-mark-command-repeat-pop t)

;; zap-up-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Sanity...
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-startup-message t
      ;; font-lock-maximum-decoration t
      ;; color-theme-is-global t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-window-setup-function 'ediff-setup-windows-plain
      ring-bell-function 'ignore  ; Never beep!
      gc-cons-threshold 50000000  ; Don't be so stingy on the memory, we have lots now. It's the distant future.
      large-file-warning-threshold 100000000  ; 100MB might be too much
      save-place-file (expand-file-name "saveplace" vlt-backups-dir)  ; Save place in buffers
      backup-directory-alist `(("." . ,vlt-backups-dir))  ; Keep things clean!
      truncate-partial-width-windows nil  ; Don't truncate lines
      redisplay-dont-pause t  ; Don't defer screen updates when performing operations
      echo-keystrokes 0.1  ; Show keystrokes in progress
      delete-by-moving-to-trash t  ; Move files to trash when deleting
      shift-select-mode nil  ; Real emacs knights don't use shift to mark things
      x-select-enable-clipboard t  ; Allow pasting selection outside of Emacs
      enable-recursive-minibuffers t  ; Allow recursive minibuffers
      )
(load custom-file)

;;; Keyboard shortcuts override
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)  ; Really Quit
(global-unset-key (kbd "C-x C-c"))  ; Avoid killing emacs so easy

(define-key key-translation-map [?\C-h] [?\C-?])  ; Use shell-like backspace C-h
(global-set-key (kbd "<f1>") 'help-command)  ; make sure <f1> keeps the help

(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

(global-set-key (kbd "C-c C-k") 'eval-buffer)

(provide 'vlt-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlt-defaults.el ends here
