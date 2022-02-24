(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval let
           ((root-dir
             (file-name-directory
              (let
                  ((d
                    (dir-locals-find-file ".")))
                (if
                    (stringp d)
                    d
                  (car d))))))
           (setq-local flycheck-css-stylelint-executable
                       (concat root-dir "node_modules/.bin/stylelint")))
     (eval progn
           (put-clojure-indent 'async 1)
           (put-clojure-indent 'car/wcar 1))
     (nrepl-use-ssh-fallback-for-remote-hosts . t)
     (cider-ns-refresh-after-fn . "user/start")
     (cider-ns-refresh-before-fn . "user/stop")
     (eval progn
           (put-clojure-indent 'async 1)
           (put-clojure-indent 'car/wcar 1))
     (header-auto-update-enabled))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
