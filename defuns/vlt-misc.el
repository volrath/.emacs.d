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
;;     Update #: 39
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
(require 's)
(require 'smartparens)
(require 'subr-x)

(defun vlt--sp-key-binding-from-command (command)
  "Return a list containing the COMMAND name and its associated key seq."
  (let ((command-name (symbol-name command)))
    (list
     command-name
     (substitute-command-keys (format "<\\[%s]>" command-name)))))

(defun vlt--sp-command-from-key-binding (key-binding)
  "Return a list containing the KEY-BINDING and its associated command."
  (list
   (lookup-key (current-global-map) (kbd key-binding))
   (format "<%s>" key-binding)))

(defun vlt--generate-sp-command-key-binding-pair (target)
  "Generate a command-keybinding pair for TARGET."
  (if (symbolp target)
      (vlt--sp-key-binding-from-command target)
    (vlt--sp-command-from-key-binding target)))

(defun vlt--display-sp-map (title commands)
  "Display bindings with TITLE for COMMANDS."
  (insert title)
  (newline 2)
  (ctbl:create-table-component-region
   :model (ctbl:make-model-from-list
           (mapcar #'vlt--generate-sp-command-key-binding-pair commands)
           '("Command" "Key Seq")))
  (newline))

(defun sp-cheatsheet ()
  "Custom smartparens cheat sheet that add keymaps at the beginning."
  (interactive)
  (sp-cheat-sheet)
  (with-current-buffer (get-buffer "*Smartparens cheat sheet*")
    (goto-char (point-min))
    (vlt--display-sp-map "Navigation"
                         '(sp-beginning-of-sexp
                           sp-end-of-sexp
                           sp-up-sexp
                           sp-backward-up-sexp
                           sp-down-sexp
                           sp-backward-down-sexp
                           sp-forward-sexp
                           sp-backward-sexp
                           sp-next-sexp
                           sp-previous-sexp))
    (vlt--display-sp-map "Killing"
                         '(sp-kill-sexp
                           sp-kill-word
                           sp-raise-sexp
                           sp-splice-sexp-killing-around
                           sp-splice-sexp
                           sp-splice-sexp-killing-forward
                           sp-splice-sexp-killing-backward))
    (vlt--display-sp-map "Wrapping"
                         '("M-("
                           "M-["
                           "M-{"
                           sp-unwrap-sexp
                           sp-backward-unwrap-sexp
                           sp-forward-slurp-sexp
                           sp-forward-barf-sexp
                           sp-backward-slurp-sexp
                           sp-backward-barf-sexp
                           sp-join-sexp
                           sp-split-sexp
                           sp-rewrap-sexps))
    (vlt--display-sp-map "Misc"
                         '(sp-select-next-thing-exchange
                           sp-select-previous-thing
                           sp-select-next-thing
                           sp-prefix-tag-object
                           sp-prefix-pair-object
                           sp-prefix-symbol-object
                           sp-highlight-current-sexp
                           sp-prefix-save-excursion
                           sp-convolute-sexp
                           sp-absorb-sexp
                           sp-emit-sexp
                           sp-add-to-previous-sexp
                           sp-add-to-next-sexp
                           sp-transpose-hybrid-sexp
                           sp-comment))
    (goto-char (point-min))))

(defun sudo-edit (&optional arg)
  "Sudo edit ARG."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun vlt-swiper-at-point (sym)
  "Use `swiper' to search for the SYM at point."
  (interactive (list (thing-at-point 'symbol)))
  (swiper sym))


(defun vlt--git-remote-url (remote-name)
  "Return the HTTP URL of REMOTE-NAME."
  (let ((url (s-chomp (shell-command-to-string (format "git config --get remote.%s.url" remote-name)))))
    (thread-last url
      (replace-regexp-in-string ":" "/")
      (replace-regexp-in-string "^git@" "https:\/\/")
      (replace-regexp-in-string ".git$" ""))))


(defun vlt--git-rev-abbrev ()
  "Return the abbreviated reference for HEAD in the current git repository."
  (s-chomp (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))


(defun vlt-github-url-at-point ()
  "Copy a Github URL link to the line where the pointer is."
  (interactive)
  (let ((line-nr (line-number-at-pos))
        (repo-url (vlt--git-remote-url "origin"))
        (git-rev (vlt--git-rev-abbrev))
        (path-to-file (s-replace (expand-file-name (vc-root-dir))
                                 ""
                                 (buffer-file-name))))
    ;; (concat repo-url "/tree/" branch "/" path-to-file "#L" line-nr)
    (kill-new (format "%s/tree/%s/%s#L%s"
                      repo-url
                      git-rev
                      path-to-file
                      line-nr))))


(defun vlt-zprint-buffer ()
  "Run zprint on current buffer and replace its content with the formatted version."
  (interactive)
  (let ((iro inhibit-read-only))
    (setq inhibit-read-only t)
    (if-let ((zprint (executable-find "zprint")))
        (shell-command-on-region (point-min)
                                 (point-max)
                                 zprint
                                 (current-buffer)
                                 t)
      (error "Can't find zprint.  Is it installed?"))
    (setq inhibit-read-only iro)))

(provide 'vlt-misc)
;;; vlt-misc.el ends here
