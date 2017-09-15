;;; vlt-config-ivy.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-config-ivy.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Fri Sep 15 10:53:02 2017 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 10
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Mostly copied from Purcell's configuration:
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-ivy.el
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

(require 'counsel)
(require 'flx)
(require 'ivy)
(require 'perspective)
(require 'smex)
(require 'swiper)

(ivy-mode t)
(counsel-mode t)

(setq-default ivy-use-virtual-buffers t
              ivy-virtual-abbreviate 'fullpath
              ivy-height 15
              ivy-count-format "(%d/%d) "
              projectile-completion-system 'ivy
              ivy-re-builders-alist '((t . ivy--regex-fuzzy))
              ivy-initial-inputs-alist '((man . "^")
                                         (woman . "^"))
              ;; Counsel
              counsel-mode-override-describe-bindings t)

;; IDO-style directory navigation
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(dolist (k '("C-j" "C-RET"))
  (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

(define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)

(diminish 'ivy-mode)
(diminish 'counsel-mode)

;; perspective
(add-hook 'ivy-ignore-buffers
          #'(lambda (b)
              (when persp-mode
                (if persp-curr
                    (not (memq b (persp-buffers persp-curr)))
                  nil))))

(setq ivy-sort-functions-alist
      (append ivy-sort-functions-alist
              '((persp-remove-buffer . nil)
                (persp-add-buffer    . nil)
                (persp-switch        . nil))))

;; smex
(setq smex-save-file (expand-file-name ".smex-items" vlt-backups-dir))
(smex-initialize)

(provide 'vlt-config-ivy)

;;; vlt-config-ivy.el ends here
