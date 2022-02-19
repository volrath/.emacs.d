;;; vlt-smartparens.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-smartparens.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sat Feb 12 21:14:26 2022 (+0100)
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

(require 'vlt-defuns)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ;; Navigation
        ("M-e"   . sp-end-of-sexp)
        ("M-a"   . sp-beginning-of-sexp)
        ("C-M-a" . sp-backward-up-sexp)
        ("C-M-e" . sp-up-sexp)
        ("M-d"   . sp-down-sexp)
        ("M-f"   . sp-forward-sexp)
        ("M-b"   . sp-backward-sexp)
        ("M-n"   . sp-next-sexp)
        ("M-p"   . sp-previous-sexp)

        ;; Editing
        ("M-k"     . sp-kill-sexp)
        ("M-r"     . sp-raise-sexp)
        ("C-)"     . sp-forward-slurp-sexp)
        ("C-("     . sp-forward-barf-sexp)
        ("C-M-("   . sp-backward-slurp-sexp)
        ("C-M-)"   . sp-backward-barf-sexp)
        ("C-x C-t" . sp-transpose-hybrid-sexp)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward))
  :defer nil
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode)

  (define-key smartparens-mode-map (kbd "M-(") (vlt/wrap-with "("))
  (define-key smartparens-mode-map (kbd "M-\"") (vlt/wrap-with "\""))

  (use-package hydra
    :ensure t
    :config
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
        ("q" nil "Quit" :color blue)))))


(provide 'vlt-smartparens)

;;; vlt-smartparens.el ends here
