2
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(require 'package)
;; intel mac (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(require 'cask "/opt/homebrew/share/emacs/site-lisp/cask/cask.el")
;; linux (require 'cask "~/.cask/cask.el")

(cask-initialize)

(load "~/.emacs.d/src/common.el")
(load "~/.emacs.d/src/clojure.el")
(load "~/.emacs.d/src/css.el")
(load "~/.emacs.d/src/elisp.el")
(load "~/.emacs.d/src/files.el")
(load "~/.emacs.d/src/git.el")
(load "~/.emacs.d/src/gui.el")
(load "~/.emacs.d/src/keybindings.el")
(load "~/.emacs.d/src/js.el")
(load "~/.emacs.d/src/markups.el")
(load "~/.emacs.d/src/typography.el")
(load "~/.emacs.d/src/utils.el")

;;(set-exec-path-from-shell)
(add-to-list 'exec-path "/usr/local/bin")

;; Temporarily reduce gc at startup
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")

(setq gc-cons-threshold (* 1024 1024 1024))

(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes '("misterioso" default))
 '(package-selected-packages
   '(solarized-theme adoc-mode gnu-elpa-keyring-update yaml-mode writeroom-mode use-package smex smart-mode-line scss-mode rainbow-delimiters projectile prodigy paredit pallet markdown-mode magit lv js2-mode idle-highlight-mode helm-dash graphql go-mode git-timemachine flycheck-cask expand-region exec-path-from-shell emmet-mode company cider))
 '(warning-suppress-types '(((package reinitialization)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
