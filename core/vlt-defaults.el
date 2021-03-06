;;; vlt-defaults.el --- -*- lexical-binding: t; -*-
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
;;     Update #: 4
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

(require 'ansi-color)
(require 'compile)
(require 'midnight)
(require 'misc)
(require 'tramp)

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

;; prettify symbols
(global-prettify-symbols-mode t)

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

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; enable which-func-mode
(which-func-mode 1)

;; set tramp max chunksize
(setq tramp-chunksize 250)
(setq tramp-default-method "ssh")

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

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(defun vlt-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))
(add-hook 'compilation-filter-hook #'vlt-colorize-compilation-buffer)

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

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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
      xterm-mouse-mode t  ; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text_002dOnly-Mouse.html
      ispell-program-name "aspell"
      version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first error
      )
(load custom-file)

(provide 'vlt-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlt-defaults.el ends here
