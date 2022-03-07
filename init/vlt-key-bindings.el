;;; vlt-key-bindings.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-key-bindings.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sun Feb 13 09:48:43 2022 (+0100)
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

;;; General Emacs Keyseq override
;;  --------------------------------------------------------------------

(require 'vlt-defuns)

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)  ; Really Quit
(global-unset-key (kbd "C-x C-c"))  ; Avoid killing emacs so easy
(global-unset-key (kbd "C-z"))  ; and also suspending
(global-unset-key (kbd "C-x C-p"))  ; this one is too close to switching persps

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

(global-set-key (kbd "C-S-f") 'forward-word)
(global-set-key (kbd "C-S-b") 'backward-word)
(global-set-key (kbd "C-S-n") 'forward-paragraph)
(global-set-key (kbd "C-S-p") 'backward-paragraph)

(global-set-key (kbd "C-S-d") 'kill-word)


;;; Completion
;;  ----------------------------------------------------------------------------

(global-set-key (kbd "C-.") 'hippie-expand)  ;; Used to be custom vlt/hippie-exapnd-no-case-fold
(global-set-key (kbd "C-,") 'completion-at-point)


;;; killing / yanking
;;  --------------------------------------------------------------------

(global-set-key (kbd "M-S-y") 'copy-from-above-command)
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-S-h") 'kill-region-or-backward-word)

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)  ;; Duplicate region


;;; Navigation
;;  --------------------------------------------------------------------

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)


;;; Editing
;;  --------------------------------------------------------------------

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)


;;; Buffers and windows
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-b") 'quick-switch-buffer)
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-x !") 'vlt/clone-window-and-delete-all-others)


;;; Random helpers
;;  --------------------------------------------------------------------

(global-set-key (kbd "C-s-v g l") 'vlt/github-url-at-point)

(provide 'vlt-key-bindings)

;;; vlt-key-bindings.el ends here
