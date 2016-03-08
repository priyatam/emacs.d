(require 'js2-mode)

;; Javascript -----

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)

(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; React -----

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
