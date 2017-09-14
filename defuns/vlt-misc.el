;;; vlt-misc.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-misc.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Wed Sep 13 11:14:02 2017 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 5
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


(require 'ctable)
(require 'dash)
(require 'smartparens)

(defun vlt-display-sp-map ()
  (newline 2)
  (open-line 2)
  (ctbl:create-table-component-region
   :model (ctbl:make-model-from-list
           (mapcar (lambda (p) (list (cdr p) (car p)))
                   sp-paredit-bindings)
           '("Function" "Key Seq"))))

(defun sp-cheatsheet ()
  "Custom smartparens cheat sheet that add keymaps at the beginning."
  (interactive)
  (sp-cheat-sheet)
  (with-current-buffer (get-buffer "*Smartparens cheat sheet*")
    (goto-char (point-min))
    (vlt-display-sp-map)))

(provide 'vlt-misc)
