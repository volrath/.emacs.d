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
