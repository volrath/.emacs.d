;;; vlt-lang-clojure.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-lang-clojure.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sat Feb 12 21:12:01 2022 (+0100)
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

(require 'vlt-defaults)

(use-package clojure-mode
  :ensure t
  :init
  (defun vlt/clj-doto-print ()
    "Wrap the sexp at point in a `(doto <sexp> <print-fn>)'.
Based on `smartparens''s `sp-get-thing', and uses a different
`print-fn' per major mode: In `clojure-mode' we use
`clojure.pprint/pprint', in `clojurescript-mode' we use
`js/console.log'.  If the major mode doesn't match any of these,
we use the generic `prn'."
    (interactive)
    (let* ((ok (sp-get-thing))
           (beg (sp-get ok :beg))
           (end (sp-get ok :end))
           (printer (cl-case major-mode
                      ('clojure-mode "clojure.pprint/pprint")
                      ('clojurescript-mode "js/console.log")
                      (t "prn"))))
      (save-excursion
        (goto-char beg)
        (let ((sexp (buffer-substring-no-properties beg end)))
          (delete-region beg end)
          (insert (format "(doto %s %s)" sexp printer))))))

  (defun vlt/zprint-buffer ()
    "Run zprint on current buffer and replace its content with the formatted version."
    (interactive)
    (let ((iro inhibit-read-only))
      (setq inhibit-read-only t)
      (if-let ((zprint (executable-find "zprint")))
          (shell-command-on-region (point-min)
                                   (point-max)
                                   zprint
                                   (current-buffer)
                                   t)
        (error "Can't find zprint.  Is it installed?"))
      (setq inhibit-read-only iro)))

  :bind (:map clojure-mode-map
              ("C-s-v C-s-l" . vlt/clj-doto-print)
              ("C-s-v C-s-z" . vlt/zprint-buffer))

  :config
  (use-package flycheck-clj-kondo :ensure t)
  (require 'flycheck-clj-kondo)

  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  ;; (add-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (diminish 'yas-minor-mode)

  (use-package cider
    :ensure t
    :init
    (add-hook 'cider-repl-mode-hook (lambda () (aggressive-indent-mode 0)))
    (defun vlt/cider-quit-all ()
      "Iterate over all CIDER connections and close them all!"
      (interactive)
      (let ((repl-buffers (seq-filter (lambda (b)
                                        (with-current-buffer b
                                          (eq major-mode 'cider-repl-mode)))
                                      (buffer-list))))
        (dolist (buf repl-buffers)
          (cider--close-connection buf))
        (message "All CIDER connections closed")))
    :bind (:map cider-mode-map ("C-c C-Q" . vlt/cider-quit-all))
    :custom
    (cider-repl-display-help-banner nil)
    (cider-repl-pop-to-buffer-on-connect nil)
    (nrepl-hide-special-buffers t)
    (nrepl-log-messages t)
    :config
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (define-key cider-repl-mode-map (kbd "C-c C-o") 'cider-repl-clear-buffer)

    (use-package cider-eval-sexp-fu :ensure t)

    (setq cider-history-file (expand-file-name "cider-history" vlt-defaults/backups-dir)
          nrepl-prompt-to-kill-server-buffer-on-quit nil))

  (use-package clj-refactor
    :ensure t
    :diminish clj-refactor-mode
    :config
    (add-hook 'clojure-mode-hook 'clj-refactor-mode)
    (add-hook 'cider-repl-mode-hook 'clj-refactor-mode)
    (cljr-add-keybindings-with-modifier "C-s-")
    (setq cljr-warn-on-eval nil
          cljr-favor-prefix-notation nil
          cljr-favor-private-functions nil
          cljr-eagerly-build-asts-on-startup nil))

  (use-package clojure-mode-extra-font-locking :ensure t))

(provide 'vlt-lang-clojure)

;;; vlt-lang-clojure.el ends here