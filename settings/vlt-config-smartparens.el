;;; vlt-config-smartparens.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-config-smartparens.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Wed Sep 13 14:33:59 2017 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 9
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Taken from @Fuco1's configuration:
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
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

(require 'smartparens-config)

(setq sp-autoescape-string-quote nil)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)

(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

;; (--each '(css-mode-hook
;;           restclient-mode-hook
;;           js-mode-hook
;;           java-mode
;;           ruby-mode
;;           markdown-mode
;;           groovy-mode
;;           scala-mode)
;;   (add-hook it 'turn-on-smartparens-mode))
;;; markdown-mode

(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*"
                 :wrap "C-*"
                 :unless '(sp--gfm-point-after-word-p sp-point-at-bol-p)
                 :post-handlers '(("[d1]" "SPC"))
                 :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "**" "**")
  (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))

(defun sp--gfm-point-after-word-p (id action context)
  "Return t if point is after a word, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "\\(\\sw\\)" (regexp-quote id)))))

(defun sp--gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

;;; rst-mode
(sp-with-modes 'rst-mode
  (sp-local-pair "``" "``"))

;;; org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "«" "»"))

(defun sp--org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(my-add-space-before-sexp-insertion)
                 :post-handlers '(my-add-space-after-sexp-insertion)))



(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))

;;; C++
(sp-with-modes '(malabar-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                    ("* ||\n[i]" "RET")))

;;; PHP
(sp-with-modes '(php-mode)
  (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             (my-php-handle-docstring "RET")))
  (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

(defun my-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ;; variable
     ((string-match (rx (or "private" "protected" "public" "var") (1+ " ") (group "$" (1+ alnum))) line)
      (let ((var-name (match-string 1 line))
            (type ""))
        ;; try to guess the type from the constructor
        (-when-let (constructor-args (my-php-get-function-args "__construct" t))
          (setq type (cdr (assoc var-name constructor-args))))
        (insert "* @var " type)
        (save-excursion
          (insert "\n"))))
     ((string-match-p "function" line)
      (save-excursion
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args nil t))))
          (--each args
            (let ((type (cond
                         ((equal (cdr it) "array") "mixed[] ")
                         ((null (cdr it))))))
              (when type
                (insert (format "* @param %s%s\n" (if (eq type t) "" type) (car it)))))))
        (let ((return-type (save-excursion
                             (forward-line)
                             (my-php-get-function-return-type))))
          (let ((type (cond
                       ((equal return-type "array") "mixed[] ")
                       ((null return-type)))))
            (when type
              (insert (format "* @return %s\n" (if (eq type t) "" type)))))))
      (re-search-forward (rx "@" (or "param" "return") " ") nil t))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))

(provide 'vlt-config-smartparens)

;;; vlt-config-smartparens.el ends here
