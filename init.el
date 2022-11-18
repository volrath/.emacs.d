;;; init.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: init.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sat Jan 29 11:35:06 2022 (+0100)
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

;;; Package Management Setup
;;  ---------------------------------------------------------------------------

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq use-package-verbose t
      use-package-always-demand t)

;; Configure use-package to use straight.el by default
(eval-when-compile (require 'use-package))
(use-package straight
  :custom (straight-use-package-by-default t))


;;; Load paths and require modules
;;; ---------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(require 'vlt-defaults)
(require 'vlt-packages)
(require 'vlt-key-bindings)
(require 'vlt-ui)

;; load private settings I would like to keep out of the public repo.
(let ((private-config (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-config) (load private-config)))

;; load local settings that each workstation might define.
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config) (load local-config)))


;;; Server
;;  ---------------------------------------------------------------------------

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
