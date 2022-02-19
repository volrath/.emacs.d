;;; vlt-key-bindings.el ---  -*- lexical-binding: t; -*-
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
;;     Update #: 36
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

(require 'flyspell)
(require 'vlt-buffer-defuns)
(require 'vlt-file-defuns)
(require 'vlt-editing-defuns)
(require 'vlt-lisp-defuns)
(require 'vlt-misc)

(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
(global-unset-key (kbd "<menu>"))
(define-key input-decode-map (kbd "<menu>") (kbd "<C-[>"))

;;; General Emacs Keyseq override
;;  --------------------------------------------------------------------
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)  ; Really Quit
(global-unset-key (kbd "C-x C-c"))  ; Avoid killing emacs so easy
(global-unset-key (kbd "C-z"))  ; and also suspending
(global-unset-key (kbd "C-x C-p"))  ; this one is too close to switching persps

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

(global-set-key (kbd "C-M-c") 'upcase-word)

(global-set-key (kbd "C-S-f") 'forward-word)
(global-set-key (kbd "C-S-b") 'backward-word)
(global-set-key (kbd "C-S-n") 'forward-paragraph)
(global-set-key (kbd "C-S-p") 'backward-paragraph)

(global-set-key (kbd "C-S-d") 'kill-word)

;;; Completion that uses many different methods to find options.
;;  --------------------------------------------------------------------
(define-key flyspell-mode-map (kbd "C-.") nil)
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)
(global-set-key (kbd "M-/") 'hippie-expand)


;;; killing / yanking
;;  --------------------------------------------------------------------
(global-set-key (kbd "M-S-y") 'copy-from-above-command)
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-S-h") 'kill-region-or-backward-word)
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
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)

(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(global-set-key (kbd "C-x C-j") 'dired-jump)

(global-set-key (kbd "M-s /") 'vlt-swiper-at-point)

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)  ;; lol

;;; Smartparens overrides
;;  -------------------------------------------------------------------

(define-key smartparens-mode-map (kbd "M-s") nil)

;; Navigation
(define-key smartparens-mode-map (kbd "M-e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "M-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)

(define-key smartparens-mode-map (kbd "M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-backward-down-sexp)

(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-symbol)
(define-key smartparens-mode-map (kbd "M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "M-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-M-s")
  (defhydra smartparens-hydra ()
    "Smartparens"
    ("d" sp-down-sexp "Down")
    ("e" sp-up-sexp "Up")
    ("u" sp-backward-up-sexp "Up")
    ("a" sp-backward-down-sexp "Down")
    ("f" sp-forward-sexp "Forward")
    ("b" sp-backward-sexp "Backward")
    ("k" sp-kill-sexp "Kill" :color blue)
    ("q" nil "Quit" :color blue)))

;; Killing
(define-key smartparens-mode-map (kbd "M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-kill-word)
(define-key smartparens-mode-map (kbd "M-r") 'sp-raise-sexp)

(define-key smartparens-mode-map (kbd "M-S-s") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

;; Wrapping
(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "H-s j") 'sp-join-sexp)
(define-key smartparens-mode-map (kbd "H-s s") 'sp-split-sexp)
(define-key smartparens-mode-map (kbd "H-s r") 'sp-rewrap-sexp)

;; Misc
(define-key flyspell-mode-map (kbd "C-;") nil)
(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "<C-[>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "H-t") 'sp-prefix-tag-object)
(define-key smartparens-mode-map (kbd "H-p") 'sp-prefix-pair-object)
(define-key smartparens-mode-map (kbd "H-y") 'sp-prefix-symbol-object)
(define-key smartparens-mode-map (kbd "H-h") 'sp-highlight-current-sexp)
(define-key smartparens-mode-map (kbd "H-e") 'sp-prefix-save-excursion)
(define-key smartparens-mode-map (kbd "H-s c") 'sp-convolute-sexp)
(define-key smartparens-mode-map (kbd "H-s a") 'sp-absorb-sexp)
(define-key smartparens-mode-map (kbd "H-s e") 'sp-emit-sexp)
(define-key smartparens-mode-map (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key smartparens-mode-map (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key smartparens-mode-map (kbd "C-x C-t") 'sp-transpose-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-;") 'sp-comment)

;; (defvar hyp-s-x-map)
;; (define-prefix-command 'hyp-s-x-map)
;; (define-key smartparens-mode-map (kbd "H-s x") hyp-s-x-map
;; (define-key smartparens-mode-map (kbd "H-s x x") 'sp-extract-before-sexp
;; (define-key smartparens-mode-map (kbd "H-s x a") 'sp-extract-after-sexp
;; (define-key smartparens-mode-map (kbd "H-s x s") 'sp-swap-enclosing-sexp

;; (define-key smartparens-mode-map (kbd [remap c-electric-backspace]) 'sp-backward-delete-char)

;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;; (define-key smartparens-mode-map (kbd "C-(") 'sp---wrap-with-40)


;;; Ivy / Counsel / Swiper - combo
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "M-s g") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "M-s l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(global-set-key (kbd "C-c C-r") 'ivy-resume)

;;; Org mode
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;;; UI
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-<f8>") 'menu-bar-mode)

(global-set-key (kbd "M-h") 'which-key-C-h-dispatch)

;;; Random
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x M") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

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
