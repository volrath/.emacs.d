;;; vlt-config-lisp.el ---
;;
;; Copyright © 2011-2017 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration shared between all modes related to lisp-like languages.

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

(require 'vlt-config-programming)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; a great lisp coding hook
(defun vlt-lisp-coding-defaults ()
  (setq whitespace-line-column 120)
  (aggressive-indent-mode t)
  (smartparens-strict-mode t))

(setq vlt-lisp-coding-hook 'vlt-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun vlt-interactive-lisp-coding-defaults ()
  (aggressive-indent-mode t)
  (smartparens-strict-mode t)
  (whitespace-mode nil))

(setq vlt-interactive-lisp-coding-hook 'vlt-interactive-lisp-coding-defaults)

(provide 'vlt-config-lisp)

;;; vlt-config-lisp.el ends here
