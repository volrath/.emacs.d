;;; vlt-company.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-company.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Mon Feb 14 20:37:01 2022 (+0100)
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

(use-package company
  :defer nil
  :custom
  (company-idle-delay 0.5)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2)
  :config
  (global-company-mode 1))

;; (use-package company-box
;;   :diminish company-box-mode
;;   :hook (company-mode . company-box-mode)
;;   ;; :init
;;   ;; (setq company-box-icons-alist 'company-box-icons-all-the-icons)
;;   ;; :config
;;   ;; (use-package all-the-icons)
;;   ;; (setf (alist-get 'min-height company-box-frame-parameters) 6)
;;   ;; (setq company-box-icons-alist 'company-box-icons-all-the-icons
;;   ;;       company-box-backends-colors nil

;;   ;;       ;; These are the Doom Emacs defaults
;;   ;;       company-box-icons-all-the-icons
;;   ;;       `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
;;   ;;         (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
;;   ;;         (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
;;   ;;         (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
;;   ;;         (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
;;   ;;         (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
;;   ;;         (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
;;   ;;         (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
;;   ;;         (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
;;   ;;         (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
;;   ;;         (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
;;   ;;         (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
;;   ;;         (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
;;   ;;         (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
;;   ;;         (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
;;   ;;         (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
;;   ;;         (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
;;   ;;         (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
;;   ;;         (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
;;   ;;         (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
;;   ;;         (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
;;   ;;         (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
;;   ;;         (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
;;   ;;         (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
;;   ;;         (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
;;   ;;         (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
;;   ;;         (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))
;;   )

;;; vlt-company.el ends here
