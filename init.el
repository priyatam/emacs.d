(require 'package)
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")

(cask-initialize)

(load "~/.emacs.d/src/clojure.el")
(load "~/.emacs.d/src/css.el")
(load "~/.emacs.d/src/elisp.el")
(load "~/.emacs.d/src/files.el")
(load "~/.emacs.d/src/git.el")
(load "~/.emacs.d/src/gui.el")
(load "~/.emacs.d/src/keybindings.el")
(load "~/.emacs.d/src/js.el")
(load "~/.emacs.d/src/markups.el")
(load "~/.emacs.d/src/presentation.el")
(load "~/.emacs.d/src/typography.el")
(load "~/.emacs.d/src/utils.el")

(set-exec-path-from-shell)
(add-to-list 'exec-path "/usr/local/bin")

;; Temporarily reduce gc at startup
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")

(setq gc-cons-threshold (* 128 1024 1024))

(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
	("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "8553bdd21237bcc7d9cfabd93c1525af9ac4a84d5ab1435e6474f9fd4f81d69c" "bf25a2d5c2eddc36b2ee6fc0342201eb04ea090e637562c95b3b6e071216b524" default)))
 '(fci-rule-color "#d6d6d6")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(jsx-indent-level 2)
 '(jsx-use-flymake t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
