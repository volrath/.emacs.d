(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#da8548" "#61afef" "#c678dd" "#1f5582" "#abb2bf"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("e39ff005e524c331b08d613109bff0b55fc21c64914c4a243faa70f330015389" "c03d60937e814932cd707a487676875457e0b564a615c1edfd453f23b06fe879" "9ff4eb11b3d72c41325d3113bac30b9613c75b7a9ba0892c10bb5b9d9691b85f" "6570843991e40121f854432826e9fd175aec6bd382ef217b2c0c46da37f3af18" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(debug-on-error t)
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-commit-arguments (quote ("--gpg-sign=6276E2C1CB4235C7")))
 '(magit-diff-use-overlays nil)
 '(magit-tag-arguments (quote ("--sign")))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files (quote ("/home/volrath/projects/life/agenda.org")))
 '(org-capture-templates
   (quote
    (("j" "Journal Entry" entry
      (file+olp+datetree "~/projects/life/journal.org")
      "* %U - %^{Headline}")
     ("t" "TODO" entry
      (file+headline "~/projects/life/agenda.org" "Tasks")
      (file "~/projects/life/todo-capture.template")))))
 '(package-selected-packages
   (quote
    (graphviz-dot-mode company company-lsp dap-mode lsp-ui lsp-java magit ivy-clojuredocs inf-clojure flycheck-clj-kondo clojure-mode counsel ivy swiper zmq rust-mode company-terraform terraform-mode go-mode wgrep-ag ivy-hydra docker ag json-mode page-break-lines unfill shell-script-mode ivy-rich cider all-the-icons notmuch flycheck-joker ibuffer-vc org htmlize ox-reveal easy-hugo a nov highlight-indent-guides company-mode company-anaconda anaconda-mode header2 persp-mode-projectile-bridge workgroups persp-mode workgroups2 yaml-mode cask-mode buttercup password-store password-store-otp zop-to-char zoom-frm zerodark-theme zencoding-mode whitespace-cleanup-mode which-key wgrep volatile-highlights visual-regexp undo-tree tagedit solarized-theme smooth-scrolling smartrep smartparens smart-mode-line smart-forward restclient rainbow-delimiters prodigy perspective peep-dired pass ov operate-on-number move-text markdown-mode macrostep js2-refactor imenu-anywhere highlight-escape-sequences helm-projectile helm-fuzzier helm-flx helm-descbinds helm-ag hardcore-mode guru-mode grizzl gitignore-mode gitconfig-mode git-timemachine gist flycheck-pos-tip flycheck-clojure find-file-in-project fill-column-indicator elisp-slime-nav editorconfig dockerfile-mode discover-my-major dired-narrow dired-k dired-details diminish diff-hl ctable csv-mode crux clojure-mode-extra-font-locking clj-refactor change-inner browse-kill-ring beginend apache-mode anzu aggressive-indent ace-window)))
 '(persp-keymap-prefix "p")
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((eval let
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
           (put-clojure-indent
            (quote async)
            1)
           (put-clojure-indent
            (quote car/wcar)
            1))
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
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
             (cider-register-cljs-repl-type
              (quote edison)
              "(start-photon!)")
             (setq-local cider-default-cljs-repl
                         (quote edison)))
           (put-clojure-indent
            (quote async)
            1)
           (put-clojure-indent
            (quote car/wcar)
            1))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (checkdoc-package-keywords-flag)
     (header-auto-update-enabled))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "#073642" :overline t))))
 '(ivy-current-match ((t (:background "#eee8d5" :underline nil :weight bold)))))
