;;; vlt-key-bindings.el --- 
;; 
;; Filename: vlt-key-bindings.el
;; Description: 
;; Author: Daniel Barreto
;; Maintainer: 
;; Created: Sat Sep  9 01:04:09 2017 (+0200)
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
;; 
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

(require 'vlt-buffer-defuns)
(require 'vlt-file-defuns)
(require 'vlt-editing-defuns)
(require 'vlt-lisp-defuns)
(require 'vlt-misc)

;;; Completion that uses many different methods to find options.
;;  --------------------------------------------------------------------
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-,") 'hippie-expand-lines)

;;; killing / yanking
;;  --------------------------------------------------------------------
(global-set-key (kbd "M-S-y") 'copy-from-above-command)
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)
;; (global-set-key (kbd "M-w") 'save-region-or-current-line)  ; editing-defuns.el

(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)
(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)

(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;;; Selecting
;;  --------------------------------------------------------------------
(global-set-key (kbd "C-'") 'er/expand-region)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "M-å") 'mc/mark-all-in-region)

;; Symbol and word specific mark-more
(global-set-key (kbd "C->") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-æ") 'mc/mark-all-words-like-this)
(global-set-key (kbd "s->") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "s-<") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "s-æ") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "H-~") 'mc/insert-numbers)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)


;;; Navigation
;;  --------------------------------------------------------------------

;; iy-go-to-char - like f in Vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)

;; Clever new lines
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Use anzu instead of regular search
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; avy
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-z") 'ace-window)

;;; Editing
;;  --------------------------------------------------------------------

(global-set-key (kbd "M-s e") 'sudo-edit)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (lambda () (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (lambda () (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (lambda () (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (lambda () (replace-region-by 's-upper-camel-case)))

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;;; Buffers and windows
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)

(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(global-set-key (kbd "C-x C-j") 'dired-jump)

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)  ;; lol


;;; UI
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-<f8>") 'menu-bar-mode)

;;; Random
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x M") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "-m") 'discover-my-major)

;; Open a smartparens cheat sheet
(global-set-key (kbd "C-ö") 'sp-cheatsheet)

(provide 'vlt-key-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlt-key-bindings.el ends here
