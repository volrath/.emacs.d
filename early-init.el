(defvar vlt/var-dir (expand-file-name (concat "var/" (system-name)) user-emacs-directory)  ;; Avoid littering
  "This folder stores variable data files for `(system-name)'. For
example save/history files, package config files, etc.")


;; Native Compilation
;; -----------------------------------------------------------------------------
;; Problems with native compilation in macos
;; https://issues.guix.gnu.org/issue/57849
(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))


(when (and (boundp 'native-comp-eln-load-path)
           (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache (expand-file-name "eln-cache" vlt/var-dir)))


;; Package Manager
;; -----------------------------------------------------------------------------
(setq package-enable-at-startup nil)  ;; Disable package.el


;; UI
;; -----------------------------------------------------------------------------
;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
