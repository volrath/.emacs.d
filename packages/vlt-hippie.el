;;; vlt-hippie.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-hippie.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Thu Feb 17 20:14:59 2022 (+0100)
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

(defvar he-search-loc-backward (make-marker))
(defvar he-search-loc-forward (make-marker))

(defun try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let (expansion)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-dabbrev-search he-search-string t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-dabbrev-search he-search-string nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          nil)
      (progn
        (he-substitute-string expansion t)
        t))))

(defun try-expand-line-closest-first (old)
  "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
        (strip-prompt (and (get-buffer-process (current-buffer))
                           comint-use-prompt-regexp
                           comint-prompt-regexp)))
    (unless old
      (he-init-string (he-line-beg strip-prompt) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

;; Hippie expand: sometimes too hip
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-closest-first
                                         try-complete-file-name
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Don't case-fold when expanding with hippe
(defun vlt/hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))

(provide 'vlt-hippie)

;;; vlt-hippie.el ends here
