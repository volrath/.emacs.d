;;; init.el --- 
;; 
;; Filename: init.el
;; Description: 
;; Author: Daniel Barreto
;; Maintainer: 
;; Created: Thu Sep  7 13:02:38 2017 (+0200)
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


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar current-user
  (getenv "USER"))

(defvar vlt-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory containing core configuration.")
(defvar vlt-settings-dir (expand-file-name "settings" user-emacs-directory)
  "Directory containing settings for most of the major modes used.")
(defvar vlt-defuns-dir (expand-file-name "defuns" user-emacs-directory)
  "Directory for personal/custom defuns and experiments.")
(defvar vlt-site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar vlt-backups-dir (expand-file-name "backups" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

;; Populate `load-path'
(defun vlt-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (project (directory-files parent-dir t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project)
      (vlt-add-subfolders-to-load-path project))))

(add-to-list 'load-path vlt-core-dir)
(add-to-list 'load-path vlt-settings-dir)
(add-to-list 'load-path vlt-defuns-dir)
(add-to-list 'load-path vlt-site-lisp-dir)
(vlt-add-subfolders-to-load-path vlt-site-lisp-dir)

;; Require core config
(require 'vlt-defaults)
(require 'vlt-packages)
(require 'vlt-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
