(require 'scss-mode)

;; SCSS -----

(setq scss-sass-command "node-sass")
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq-default scss-compile-at-save nil)
