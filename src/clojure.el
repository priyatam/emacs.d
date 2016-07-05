(require 'cider-mode)
(require 'clojure-mode)
(require 'clj-refactor)
(require 'flycheck)
(require 'flycheck-clojure)
(require 'flycheck-pos-tip)
(require 'company)

;; Cider

(global-company-mode)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'my-clojure-mode-hook)

;;(setq cider-auto-mode nil)
(setq cider-interactive-eval-result-prefix ";; => ")
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-overlays-use-font-lock t)
;;(setq cider-pprint-fn "puget.printer/pprint")
(setq cider-prompt-for-symbol nil)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-display-help-banner nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-stacktrace-default-filters '(java repl tooling dup))
(setq cider-switch-to-repl-command #'cider-switch-to-current-repl-buffer)
(setq nrepl-hide-special-buffers t)
(setq nrepl-log-messages nil)

(show-paren-mode 1)

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Clojure

(add-to-list 'auto-mode-alist '("\\.boot\\.cljs\\.cljc'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\'" . clojure-mode))

(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'clojurescript-mode-hook (lambda () (whitespace-mode t)))
(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 84)

;; Coding Standards

(define-clojure-indent
  (expect 'defun)
  (expect-let 'defun)
  (given 'defun)
  (context 1)
  (freeze-time 1)
  (redef-state 1)
  (from-each 1)
  (component 'defun)
  (div 'defun)
  (span 'defun)
  (form 'defun)
  (a 'defun)
  (ul 'defun)
  (li 'defun)
  (input 'defun)
  (h1 'defun)
  (h2 'defun)
  (h3 'defun)
  (h4 'defun)
  (h5 'defun)
  (h6 'defun)
  (facts 'defun)
  (fact 'defun)
  (doto 'cond)
  (deftest 'defun)
  (testing 'defun)
  (button 'defun)
  (textarea 'defun))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

;; Flycheck -----

(eval-after-load 'flycheck '(flycheck-clojure-setup))
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;(eval-after-load 'flycheck
;;  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
