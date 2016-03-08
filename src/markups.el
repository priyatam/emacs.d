(require 'markdown-mode)
(require 'yaml-mode)

;; Markdown -----

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Yaml -----

(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
