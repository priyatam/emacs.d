;; REPOSITORIES

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

;; PATH

(defun set-exec-path-from-shell-PATH ()
 (interactive)
 (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
   (setenv "PATH" path-from-shell)
   (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;;(add-to-list 'exec-path "/usr/local/bin")

;; FONTS
(set-default-font "Menlo 16")

;; STARTUP
(setq default-directory "~/github/priyatam")

(toggle-frame-fullscreen)

;; DISABLE SOUNDS 
(setq ring-bell-function 'ignore)

;; THEMES
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; CIDER
(setq cider-repl-pop-to-buffer-on-connect nil)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-stacktrace-default-filters '(java repl tooling dup))
(setq cider-repl-display-in-current-window t)

; BUGS?
;(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
;(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Popping-up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; SCROLL 
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; File Browser / Navigator
(load "~/.emacs.d/elpa/neotree-0.2.1/neotree.el")

;; Paredit / Rainbow Parens
(add-hook 'clojure-mode-hook 'paredit-mode)
(show-paren-mode 1)
;(global-rainbow-delimiters-mode)

;; Key Bindings
(global-set-key [f1] 'neotree-dir)
(global-set-key [f2] 'cider-jack-in)
(global-set-key [f3] 'cider-switch-to-repl-buffer)
(global-set-key [f6] 'paredit-mode)

(require 'bind-key)
(bind-key "C-x z" 'cider-eval-last-sexp)
(bind-key "C-x x" 'cider-pprint-eval-defun-at-point)

;; Clojure Standards
(require 'whitespace)
(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'clojurescript-mode-hook (lambda () (whitespace-mode t)))
(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 84)



