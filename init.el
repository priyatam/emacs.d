;; REPOSITORIES

(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'cider)
  (package-install 'cider))
(unless (package-installed-p 'paredit)
  (package-install 'paredit))
(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))
(unless (package-installed-p 'company)
  (package-install 'company))
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(unless (package-installed-p 'magit)
  (package-install 'magit))
(unless (package-installed-p 'whitespace)
  (package-install 'whitespace))
(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))
(unless (package-installed-p 'scss-mode)
  (package-install 'scss-mode))
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(unless (package-installed-p 'neotree)
  (package-install 'neotree))
;(unless (package-installed-p 'flycheck-clojure)
;  (package-install 'flycheck-clojure))

;; STARTUP

(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell
	 (replace-regexp-in-string "[ \t\n]*$" ""
				   (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(add-to-list 'exec-path "/usr/local/bin")

;; (setq default-directory "~/github/")

;; FONTS

(set-default-font "Source Code Pro 18")
(setq-default line-spacing 4)

;; GUI

;; no splash screen
(setq inhibit-startup-message t)

(toggle-frame-fullscreen)

;; turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (if (fboundp 'fringe-mode) (fringe-mode 0))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq default-cursor-type 'bar)
(set-cursor-color "#ffffff") 

(global-linum-mode)

;; THEMES

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

(require 'color-theme)
(color-theme-initialize)

;; SOUNDS

(setq ring-bell-function 'ignore)

;; FILES

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ido
(global-set-key (kbd "C-x f") 'find-file-in-project)

;; dired
(global-set-key (kbd "C-x C-j") 'dired-jump)
(define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)

;; refresh files
(global-auto-revert-mode t)

;; clean backup files
(setq backup-directory-alist
     `(("." . ,(expand-file-name "~/emacs.d/backups"))))
(setq auto-save-file-name-transforms
     `((".*" ,(expand-file-name "~/emacs.d/backups") t)))

(fset 'gui-diff-last-failure
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 97 99 116 117 97 108 58 13 134217734 19 40 61 13 right 201326624 201326624 134217847 134217790 40 103 117 105 45 100 105 102 102 32 25 41] 0 "%d")) arg)))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; NAVIGATION

;; autocomplete finder
(add-hook 'ido-setup-hook
	  (lambda ()
	    ;; avoiding need to use arrow keys!
	    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
	    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point nil)

(winner-mode)

;; GIT/MAGIT

(setq magit-highlight-whitespace nil)

(global-set-key (kbd "C-c g") 'magit-status)

;; CIDER

(require 'cider-mode)

(global-company-mode)

(add-hook 'after-init-hook 'global-company-mode)


(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)

(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-stacktrace-default-filters '(java repl tooling dup))
(setq cider-repl-display-in-current-window t)
(setq cider-switch-to-repl-command #'cider-switch-to-current-repl-buffer)

;; switch current buffer into repl
(setq cider-repl-display-in-current-window t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)

(show-paren-mode 1)

;; popup contextual docs
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Clojure Coding Standards

(require 'whitespace)

(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'clojurescript-mode-hook (lambda () (whitespace-mode t)))
(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 84)

(define-clojure-indent
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
 (button 'defun)
 (textarea 'defun))

(setq-default fill-column 80)

;; Static Code Analyzer

;;(eval-after-load 'flycheck '(flycheck-clojure-setup))
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;; Hoplon

(add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\'" . clojure-mode))

;; Javascript

(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(setq js-indent-level 2)

;; (add-hook 'js-mode-hook 'js2-minor-mode)

;; Coffeescript

;;(require 'coffee-mode)

;; (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

;; CSS3/SCSS

(setq css-indent-offset 2)

;; HTML/Templates

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; Markdown
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; KEY BINDINGS

(global-set-key [f1] 'neotree-dir)
(global-set-key [f2] 'cider-jack-in)
(global-set-key [f3] 'cider-switch-to-repl-buffer)

(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "C-c C-r") 'rename-sgml-tag)

(global-set-key [remap goto-line] 'goto-line)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x f") 'sudo-find-file)

(global-set-key (kbd "<S-up>")    'windmove-up)
(global-set-key (kbd "<S-down>")  'windmove-down)
(global-set-key (kbd "<S-left>")  'windmove-left)
(global-set-key (kbd "<S-right>") 'windmove-right)

;; CUSTOM FUNCTIONS

(defun hello-world (msg)
  (interactive "sMessage: ")
  (message msg))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
  Does not indent buffer, because it is used for a before-save-hook, and that
  might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
