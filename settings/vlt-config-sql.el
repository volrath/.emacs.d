;;; vlt-config-sql.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-config-sql.el
;; Description:
;; Author:
;; Maintainer:
;; Copyright (C) 2020 Daniel Barreto
;; Created: Sun Oct 11 12:38:06 2020 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 2
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'sql)
(require 'sqlformat)

(setq sqlformat-command 'pgformatter)
(define-key sql-mode-map (kbd "C-c C-f") 'sqlformat)

(provide 'vlt-config-sql)

;;; vlt-config-sql.el ends here
