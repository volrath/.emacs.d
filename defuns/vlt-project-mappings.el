;;; vlt-project-mappings.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-project-mappings.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Tue Sep 12 15:29:01 2017 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 47
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

(require 'dash)
(require 'perspective)

(require 'vlt-python)

(defun vlt--random-perspective (nr)
  "Create the structure for a random perspective with number NR."
  (let* ((s-nr (number-to-string nr))
         (tag (intern (concat "random-" s-nr))))
    `(,tag . ((:name . ,(concat "Random " s-nr))
              (:key . ,s-nr)
              (:file . "~/projects/")))))

(defvar vlt-perspectives
  `((oss .  ((:name . "OSS")
             (:key . "o")
             (:file . "~/projects/oss/")))
    (test . ((:name . "Tests")
             (:key . "t")
             (:file . "~/projects/tests/")))
    (emacs . ((:name . "Emacs")
              (:key . "e")
              (:file . "~/.emacs.d/")))
    (swsca . ((:name . "SWSCA")
              (:key . "SPC s")
              (:file . "~/projects/swsca/backend/")
              (:config . ((vlt-python-jack-in "~/.virtualenvs/backend--s12IZY0")))))
    ,(vlt--random-perspective 1)
    ,(vlt--random-perspective 2)
    ,(vlt--random-perspective 3))
  "A collection of places I usually go to.")

(defvar vlt-last-perspective nil
  "Last perspective used.")

(defun vlt-switch-to-last-perspective ()
  "Switch to last perspective."
  (interactive)
  (if vlt-last-perspective
      (persp-switch (persp-name persp-last))
    (error "No last perspective")))

(defun vlt--perspective-command-sym (persp)
  "Generate a command sym for PERSP."
  (let ((tag (car persp)))
    (intern (concat "vlt-persp-" (symbol-name tag)))))

(defun vlt--perspective-key-seq (persp)
  "Generate a key sequence for PERSP."
  (let ((key (alist-get ':key (cdr persp))))
    (concat "C-x p " key)))

(defun vlt-bind-perspective (persp)
  "Bind PERSP to its corresponding command."
  (let ((key-seq (vlt--perspective-key-seq persp))
        (command-sym (vlt--perspective-command-sym persp)))
    (global-set-key (kbd key-seq) command-sym)))

(defun vlt--generate-persp-command (persp)
  "Break a PERSP into small pieces and create a command."
  (let* ((command-sym (vlt--perspective-command-sym persp))
         (attrs (cdr persp))
         (name   (alist-get ':name   attrs))
         (file   (alist-get ':file   attrs))
         (config (alist-get ':config attrs)))
    `(defun ,command-sym ()
       (interactive)
       (let ((initialize-p  (not (gethash ,name perspectives-hash)))
             (current-persp persp-curr))
         (persp-switch ,name)
         (when initialize-p
           (find-file ,file)
           ,@config)))))

(eval-when-compile
  (defmacro vlt-config-perspectives ()
    "Set up all project mappings."
    `(progn
       (global-unset-key (kbd "C-x p"))
       (global-set-key (kbd "C-x p -") #'vlt-switch-to-last-perspective)
       ,@(-map #'vlt--generate-persp-command vlt-perspectives)
       (-map #'vlt-bind-perspective vlt-perspectives))))

;; Advice to maintain link to previous perspective
(defun vlt--save-previous-perspective (_)
  "Advice for `persp-switch' to save current persp before moving to the next."
  (setq vlt-last-perspective persp-curr))

(advice-add #'persp-switch :before #'vlt--save-previous-perspective)

(provide 'vlt-project-mappings)

;;; vlt-project-mappings.el ends here
