(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval progn
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (eval progn (put-clojure-indent 'm/=> 1) (put-clojure-indent 'meander/match 1))
     (eval progn (put-clojure-indent 'm/=> 1))
     (eval progn (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares
                        "shadow.cljs.devtools.server.nrepl/middleware"))
     (eval progn (put-clojure-indent 'measure 2))
     (vc-prepare-patches-separately) (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-ns-refresh-after-fn . "user/resume!")
     (cider-ns-refresh-before-fn . "user/suspend!")
     (eval progn (put-clojure-indent 'async 1)) (cider-repl-use-pretty-printing)
     (eval progn (define-clojure-indent (-> 1) (->> 1)))
     (aggressive-indent-mode) (cider-ns-refresh-after-fn . "mount.core/start")
     (cider-ns-refresh-before-fn . "mount.core/stop")
     (eval progn (put-clojure-indent 'async 1) (put-clojure-indent 'car/wcar 1)
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares
                        "shadow.cljs.devtools.server.nrepl/middleware"))
     (sql-connection-alist
      (rh-test (sql-product 'postgres) (sql-port 5430) (sql-server "localhost")
               (sql-user "postgres") (sql-database "rh")))
     (sql-postgres-login-params quote
                                ((user :default "postgres")
                                 (database :default "rh")
                                 (server :default "localhost") (port 5430)))
     (sql-postgres-login-params quote
                                ((user :default "postgres")
                                 (database :default "rh")
                                 (server :default "localhost")
                                 (port :default 5430)))
     (cider-ns-refresh-after-fn . "mount.core/start")
     (cider-ns-refresh-before-fn . "mount.core/stop")
     (sql-postgres-login-params
      '((user :default "postgres") (database :default "rh")
        (server :default "localhost") (port :default 5430)))
     (eval let
           ((root-dir
             (file-name-directory
              (let ((d (dir-locals-find-file "."))) (if (stringp d) d (car d))))))
           (setq-local flycheck-css-stylelint-executable
                       (concat root-dir "node_modules/.bin/stylelint")))
     (eval progn (put-clojure-indent 'async 1) (put-clojure-indent 'car/wcar 1))
     (nrepl-use-ssh-fallback-for-remote-hosts . t)
     (cider-ns-refresh-after-fn . "user/start")
     (cider-ns-refresh-before-fn . "user/stop")
     (eval progn (put-clojure-indent 'async 1) (put-clojure-indent 'car/wcar 1))
     (header-auto-update-enabled))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
