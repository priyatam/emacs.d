(require 'emmet-mode)
(require 'web-mode)
(require 'paredit)

;; CSS -----

(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(setq auto-mode-alist
      (append '(("\\.css$" . css-mode)) auto-mode-alist))


;; HTML -----

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

;; Emmet -----

(add-hook 'sgml-mode-hook 'emmet-mode)

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(setq emmet-move-cursor-between-quotes t)
