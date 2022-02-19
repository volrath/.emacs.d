(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auth-source-save-behavior nil)
 '(cljr-eagerly-build-asts-on-startup nil)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "d88049c628f3a8a92f9e46982d3e891867e4991de2b3a714f29f9f5eb91638c1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "e39ff005e524c331b08d613109bff0b55fc21c64914c4a243faa70f330015389" "c03d60937e814932cd707a487676875457e0b564a615c1edfd453f23b06fe879" "9ff4eb11b3d72c41325d3113bac30b9613c75b7a9ba0892c10bb5b9d9691b85f" "6570843991e40121f854432826e9fd175aec6bd382ef217b2c0c46da37f3af18" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(debug-on-error t)
 '(fci-rule-color "#073642")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hl-sexp-background-color "#1c1f26")
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-commit-arguments '("--gpg-sign=6276E2C1CB4235C7"))
 '(magit-diff-use-overlays nil)
 '(magit-tag-arguments '("--sign"))
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files '("/home/volrath/projects/life/agenda.org"))
 '(org-capture-templates
   '(("j" "Journal Entry" entry
      (file+olp+datetree "~/projects/life/journal.org")
      "* %U - %^{Headline}")
     ("t" "TODO" entry
      (file+headline "~/projects/life/agenda.org" "Tasks")
      (file "~/projects/life/todo-capture.template"))))
 '(package-selected-packages
   '(2048-game typescript-mode racket-mode company-box sqlformat vega-view graphviz-dot-mode company company-lsp dap-mode lsp-ui lsp-java magit ivy-clojuredocs inf-clojure flycheck-clj-kondo clojure-mode counsel zmq rust-mode company-terraform terraform-mode go-mode wgrep-ag docker ag json-mode page-break-lines unfill shell-script-mode cider all-the-icons notmuch flycheck-joker ibuffer-vc org htmlize ox-reveal easy-hugo a nov highlight-indent-guides company-mode company-anaconda anaconda-mode header2 persp-mode-projectile-bridge workgroups persp-mode workgroups2 yaml-mode cask-mode buttercup password-store password-store-otp zop-to-char zoom-frm zerodark-theme zencoding-mode whitespace-cleanup-mode which-key wgrep volatile-highlights visual-regexp undo-tree tagedit solarized-theme smooth-scrolling smartrep smartparens smart-mode-line smart-forward restclient rainbow-delimiters prodigy perspective peep-dired pass ov operate-on-number move-text markdown-mode macrostep js2-refactor imenu-anywhere highlight-escape-sequences helm-projectile helm-fuzzier helm-flx helm-descbinds helm-ag hardcore-mode guru-mode grizzl gitignore-mode gitconfig-mode git-timemachine gist flycheck-pos-tip flycheck-clojure find-file-in-project fill-column-indicator elisp-slime-nav editorconfig dockerfile-mode discover-my-major dired-narrow dired-k dired-details diminish diff-hl ctable csv-mode crux clojure-mode-extra-font-locking clj-refactor change-inner browse-kill-ring beginend apache-mode anzu aggressive-indent ace-window))
 '(persp-keymap-prefix "p")
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   '((eval progn
           (put-clojure-indent 'async 1)
           (put-clojure-indent 'car/wcar 1)
           (put-clojure-indent 'tc\.props/for-all 1)
           (put-clojure-indent 'clojure\.test\.check\.properties/for-all 1)
           (put-clojure-indent 'tc/quick-check 1)
           (put-clojure-indent 'clojure\.test\.check/quick-check 1))
     (eval progn
           (put-clojure-indent 'async 1)
           (put-clojure-indent 'car/wcar 1)
           (put-clojure-indent 'tc\.props/for-all 1)
           (put-clojure-indent 'clojure\.test\.check\.properties/for-all 1))
     (cljr-eagerly-build-asts-on-startup)
     (eval progn
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (checkdoc-symbol-words quote
                            ("UNREPL" "unrepl" "stdout" "print-level" "print-length"))
     (eval let
           ((root-dir
             (file-name-directory
              (let
                  ((d
                    (dir-locals-find-file ".dir-locals.el")))
                (if
                    (stringp d)
                    d
                  (car d))))))
           (setq vega-view--vega-svg-command
                 (concat root-dir "node_modules/.bin/vl2svg")))
     (eval let
           ((root-dir
             (file-name-directory
              (let
                  ((d
                    (dir-locals-find-file ".dir-locals.el")))
                (if
                    (stringp d)
                    d
                  (car d))))))
           (setq-local vega-view--vega-svg-command
                       (concat root-dir "node_modules/.bin/vl2svg")))
     (eval let
           ((root-dir
             (file-name-directory
              (let
                  ((d
                    (dir-locals-find-file ".")))
                (if
                    (stringp d)
                    d
                  (car d))))))
           (setq-local vega-view--vega-svg-command
                       (concat root-dir "node_modules/.bin/vl2svg")))
     (eval let
           ((root-dir
             (file-name-directory
              (let
                  ((d
                    (dir-locals-find-file
                     (buffer-file-name))))
                (if
                    (stringp d)
                    d
                  (car d))))))
           (setq-local flycheck-python-flake8-executable
                       (concat root-dir ".venv/bin/flake8")))
     (nrepl-force-ssh-for-remote-hosts . t)
     (cider-clojure-cli-global-options . "-A:dev:test:package")
     (eval let
           ((root-dir
             (file-name-directory
              (let
                  ((d
                    (dir-locals-find-file ".")))
                (if
                    (stringp d)
                    d
                  (car d))))))
           (setq-local flycheck-python-flake8-executable
                       (concat root-dir ".venv/bin/flake8")))
     (eval progn
           (put-clojure-indent 'async 1)
           (put-clojure-indent 'car/wcar 1))
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (eval let
           ((root-dir
             (file-name-directory
              (let
                  ((d
                    (dir-locals-find-file ".")))
                (if
                    (stringp d)
                    d
                  (car d))))))
           (setq-local flycheck-python-flake8-executable
                       (concat root-dir "zamenis/.venv/bin/flake8")))
     (eval let
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
     (cider-ns-refresh-after-fn . "user/start")
     (cider-ns-refresh-before-fn . "user/stop")
     (nrepl-use-ssh-fallback-for-remote-hosts . t)
     (eval progn
           (when
               (string-prefix-p "0.18" cider-version)
             (cider-register-cljs-repl-type 'edison "(start-photon!)")
             (setq-local cider-default-cljs-repl 'edison))
           (put-clojure-indent 'async 1)
           (put-clojure-indent 'car/wcar 1))
     (eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1))
     (checkdoc-package-keywords-flag)
     (header-auto-update-enabled)))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tramp-verbose 6)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
