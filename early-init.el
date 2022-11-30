;; Native Compilation
;; -----------------------------------------------------------------------------
;; Problems with native compilation in macos
;; https://issues.guix.gnu.org/issue/57849
(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))


;; Package Manager
;; -----------------------------------------------------------------------------
(setq package-enable-at-startup nil)  ;; Disable package.el


;; UI
;; -----------------------------------------------------------------------------
;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
