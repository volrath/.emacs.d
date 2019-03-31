;;; vlt-config-clojure.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-config-clojure.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sun Sep 17 22:01:47 2017 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 22
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

(require 'vlt-config-lisp)
(vlt-require-packages '(clojure-mode cider clj-refactor flycheck-joker))
(require 'flycheck-joker)

;; global cljr config

(cljr-add-keybindings-with-modifier "C-s-")
(setq cljr-warn-on-eval nil)
(setq cljr-favor-prefix-notation nil)
(setq cljr-favor-private-functions nil)

;; cider config
(setq cider-history-file (expand-file-name "nrepl-history" vlt-backups-dir))
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-use-pretty-printing t)

;; Quit all cider connections!!
(defun vlt-cider-quit-all ()
  "Iterate over all CIDER connections and close them all!"
  (interactive)
  (let ((repl-buffers (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (eq major-mode 'cider-repl-mode)))
                                  (buffer-list))))
    (dolist (buf repl-buffers)
      (cider--close-connection buf))
    (message "All CIDER connections closed")))

;; Warn about missing nREPL instead of doing stupid things
(defun nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-z") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-k") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-n") 'nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-q") 'nrepl-warn-when-not-connected)

;; custom cider shortcuts
(define-key cider-mode-map (kbd "C-c C-e") 'cider-eval-last-sexp-and-replace)
(define-key cider-mode-map (kbd "C-c C-q") 'nrepl-close)
(define-key cider-mode-map (kbd "C-c C-Q") 'vlt-cider-quit-all)


;; setup flycheck / squiggly support
;; https://github.com/clojure-emacs/squiggly-clojure
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))

(eval-after-load 'clojure-mode
  '(progn
     (defun vlt-clojure-mode-defaults ()
       (subword-mode t)
       (clj-refactor-mode t)
       (yas-minor-mode t)
       (run-hooks 'vlt-lisp-coding-hook))

     (setq vlt-clojure-mode-hook
           'vlt-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
                                    (run-hooks 'vlt-clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)

     (add-hook 'cider-mode-hook 'eldoc-mode)

     (defun vlt-cider-repl-mode-defaults ()
       (subword-mode +1)
       (clj-refactor-mode t)
       (yas-minor-mode t)
       (run-hooks 'vlt-interactive-lisp-coding-hook))

     (setq vlt-cider-repl-mode-hook 'vlt-cider-repl-mode-defaults)

     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (run-hooks 'vlt-cider-repl-mode-hook)))))

(provide 'vlt-config-clojure)
;;; vlt-config-clojure.el ends here
