;;; vlt-config-helm.el --- Helm setup
;;
;; Copyright © 2011-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some configuration for Helm following this guide:
;; http://tuhdo.github.io/helm-intro.html

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

(vlt-require-packages '(helm helm-projectile helm-descbinds helm-ag))

(require 'helm-config)
(require 'helm-fuzzier)
(require 'helm-projectile)
(require 'helm-eshell)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
;; discussion of these options.
(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t
      helm-ag-fuzzy-match                   t
      helm-apropos-fuzzy-match              t
      helm-buffer-list-fuzzy-match          t
      helm-completing-read-fuzzy-match      t
      helm-completion-in-region-fuzzy-match t
      helm-eshell-fuzzy-match               t
      helm-etags-fuzzy-match                t
      helm-ff-fuzzy-matching                t
      helm-file-cache-fuzzy-match           t
      helm-find-files-fuzzy-match           t
      helm-imenu-fuzzy-match                t
      helm-info-emacs-fuzzy-match           t
      helm-locate-fuzzy-match               t
      helm-locate-library-fuzzy-match       t
      helm-M-x-fuzzy-match                  t
      helm-mini-fuzzy-match                 t
      helm-mode-fuzzy-match                 t
      helm-projectile-fuzzy-match           t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-session-fuzzy-match              t
      )

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

;; Use it everywhere
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(global-set-key (kbd "C-c f") 'helm-recentf)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "DEL") 'helm-find-files-up-one-level)
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

;; shell history.
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(setq projectile-completion-system 'helm)
(helm-descbinds-mode)
(helm-mode t)
(helm-fuzzier-mode t)

;; enable Helm version of Projectile with replacment commands
(helm-projectile-on)

;; display always at the bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(provide 'vlt-config-helm)

;;; vlt-config-helm.el ends here
