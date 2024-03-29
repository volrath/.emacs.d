;;; vlt-defuns.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: vlt-defuns.el
;; Author: Daniel Barreto Nava
;; Copyright (C) 2022 Daniel Barreto
;; Created: Sun Feb 13 13:43:53 2022 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Mostly copied from Magnars :)
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

(require 's)


;;; Buffer/Windows-related defuns
;;  ----------------------------------------------------------------------------

;;;###autoload
(defun split-window-right-and-move-there-dammit ()
  "You heard me!"
  (interactive)
  (split-window-right)
  (windmove-right))


;;;###autoload
(defun vlt/clone-window-and-delete-all-others ()
  "Close all other windows and clone the current one by placing it right"
  (interactive)
  (delete-other-windows)
  (split-window-right))


;;;###autoload
(defun toggle-window-split ()
  "Toggle between vertical and horizontal split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;;###autoload
(defun rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun untabify-buffer ()
  "Remove tabs from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent everything in buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))



;;; Files
;;  ----------------------------------------------------------------------------

;;;###autoload
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


;;;###autoload
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))



;;; Misc
;;  ----------------------------------------------------------------------------

;;;###autoload
(defun toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
      (let ((old-quotes (char-to-string (current-quotes-char)))
            (new-quotes (char-to-string (alternate-quotes-char)))
            (start (make-marker))
            (end (make-marker)))
        (save-excursion
          (move-point-forward-out-of-string)
          (backward-delete-char 1)
          (set-marker end (point))
          (insert new-quotes)
          (move-point-backward-out-of-string)
          (delete-char 1)
          (insert new-quotes)
          (set-marker start (point))
          (replace-string new-quotes (concat "\\" new-quotes) nil start end)
          (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))


;;;###autoload
(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation."
  (interactive)
  (back-to-indentation)
  (kill-line))


;;;###autoload
(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))


;;;###autoload
(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))


;;;###autoload
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))


(defun vlt/github-url-at-point (&optional revision args)
  (interactive (if current-prefix-arg
                   (list (magit-read-other-branch-or-commit "Revision")
                         (magit-branch-arguments))
                 (list (magit-get-current-branch))))
  (let* ((remote-url-http (thread-last (s-chomp (shell-command-to-string (format "git config --get remote.%s.url" "origin")))
                                       (replace-regexp-in-string ":" "/")
                                       (replace-regexp-in-string "^git@" "https:\/\/")
                                       (replace-regexp-in-string ".git$" "")))
         (path-to-file (s-replace (expand-file-name (vc-root-dir))
                                  ""
                                  (buffer-file-name)))
         (line-hash (if (not (use-region-p))
                        (format "#L%s" (line-number-at-pos))
                      (let* ((region-start-line (line-number-at-pos (region-beginning)))
                             (region-end-line   (if (save-excursion (goto-char (region-end)) (bolp))
                                                    (1- (line-number-at-pos (region-end)))
                                                  (line-number-at-pos (region-end)))))
                        (if (eq region-start-line region-end-line)
                            (format "#L%s" (line-number-at-pos))
                          (format "#L%s-L%s" region-start-line region-end-line)))))
         (repo-link (format "%s/tree/%s/%s%s"
                            remote-url-http
                            revision
                            path-to-file
                            line-hash)))
    (kill-new repo-link)
    (message "Copied to clipboard: %s" repo-link)))


(defun vlt/discord-time (datetime &optional output-format)
  (interactive (list (org-read-date nil t)
                     (when current-prefix-arg
                       (completing-read "Output Format: "
                                        '("d" "D" "f" "F" "R" "t" "T")
                                        nil t))))
  (kill-new (format "<t:%s:%s>" (format-time-string "%s" datetime) (or output-format "F")))
  (message "Discord date copied to clipboard"))


(when (and (boundp 'module-file-suffix) module-file-suffix)
  (require 'vterm)

  (defun vlt/vterm-run-async (shell-cmd &optional env foreground)
    "Execute SHELL-CMD and display output in a new `vterm' buffer.
Optionally you can pass an ENV list (similar to
`process-environment') to be appended to the vterm environment,
see `vterm-environment'."
    (if (not (fboundp 'vterm-other-window))
        (error "The vterm package is not installed")
      (let* ((vterm-shell shell-cmd)
             (vterm-kill-buffer-on-exit nil)
             (vterm-buffer-name (generate-new-buffer-name
                                 (concat "[vlt/vterm-run-async] " shell-cmd)))
             (display-buffer-fn (if foreground
                                    #'display-buffer
                                  (lambda (b)
                                    (message
                                     (concat "Process running in background: " vterm-buffer-name)))))
             (buf (if (not env)
                      (vterm--internal display-buffer-fn)
                    (with-current-buffer (get-buffer-create vterm-buffer-name)
                      (let ((process-environment (append env process-environment)))
                        (vterm--internal display-buffer-fn))))))
        (with-current-buffer buf
          (setq-local vlt/vterm-run-async-buffer-p t))))))

(provide 'vlt-defuns)

;;; vlt-defuns.el ends here
