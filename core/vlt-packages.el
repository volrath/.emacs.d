;;; vlt-packages.el ---
;;
;; Filename: vlt-packages.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Created: Thu Sep  7 17:04:10 2017 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Taken from the awesome `prelude-packages'
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

(require 'cl)
(require 'package)

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; set package-user-dir to be relative to Vlt install path
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(defvar vlt-packages
  '(ace-window
    all-the-icons
    anzu
    aggressive-indent
    apache-mode
    avy
    beginend
    browse-kill-ring
    cider
    change-inner  ;; TODO
    clj-refactor
    clojure-mode
    clojure-mode-extra-font-locking
    crux
    dash
    discover-my-major
    diff-hl
    diminish
    dired-details
    dired-k
    dired-narrow
    dockerfile-mode
    editorconfig
    edn
    elisp-slime-nav
    epl
    expand-region
    f
    find-file-in-project
    fill-column-indicator
    flycheck
    flycheck-clojure
    flycheck-pos-tip
    gist
    git-timemachine
    gitconfig-mode
    gitignore-mode
    grizzl
    guru-mode
    hardcore-mode
    helm
    helm-fuzzier
    helm-projectile
    highlight-escape-sequences
    hydra
    imenu-anywhere
    js2-mode
    js2-refactor
    magit
    markdown-mode
    move-text
    multiple-cursors
    operate-on-number
    ov
    pass
    peep-dired
    persp-mode
    prodigy
    projectile
    rainbow-delimiters
    restclient
    s
    smart-forward  ;; TODO
    smart-mode-line
    smartparens
    smartrep
    smooth-scrolling
    solarized-theme
    tagedit
    undo-tree
    visual-regexp
    volatile-highlights
    wgrep
    which-key
    whitespace-cleanup-mode
    yasnippet
    zencoding-mode
    zerodark-theme
    zoom-frm
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(defun vlt-packages-installed-p ()
  "Check if all packages in `vlt-packages' are installed."
  (every #'package-installed-p vlt-packages))

(defun vlt-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package vlt-packages)
    (add-to-list 'vlt-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun vlt-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'vlt-require-package packages))

(define-obsolete-function-alias 'vlt-ensure-module-deps 'vlt-require-packages)

(defun vlt-install-packages ()
  "Install all packages listed in `vlt-packages'."
  (unless (vlt-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Vlt is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (vlt-require-packages vlt-packages)))

;; run package installation
(vlt-install-packages)

(defun vlt-list-foreign-packages ()
  "Browse third-party packages not bundled with Vlt.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `vlt-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list vlt-packages)))

(defmacro vlt-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar vlt-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (vlt-auto-install extension package mode))))
 vlt-auto-install-alist)

(provide 'vlt-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlt-packages.el ends here
