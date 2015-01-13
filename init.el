;; REPOSITORIES

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; PATH

(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(add-to-list 'exec-path "/usr/local/bin")

;; (setq default-directory "~/github/")

;; FONTS ;;

(set-default-font "Source Code Pro 18")
(setq-default line-spacing 4)

;; GUI

;; No splash screen
(setq inhibit-startup-message t)

(toggle-frame-fullscreen)

;; Turn off mouse interface early in startup to avoid momentary display
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

;; SOUNDS

(setq ring-bell-function 'ignore)

;; NAVIGATION

;; File Browser
(load "~/.emacs.d/elpa/neotree-0.2.1/neotree.el")

;; Autocomplete finder
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

;; CIDER ;;

(require 'cider-mode)

;; (global-company-mode)

(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-stacktrace-default-filters '(java repl tooling dup))
(setq cider-repl-display-in-current-window t)

;; (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(show-paren-mode 1)

;; Popup contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Clojure Standards

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

;; FILES

;; Refresh files
(global-auto-revert-mode t)

;; Clean backup files
(setq backup-directory-alist
     `(("." . ,(expand-file-name "~/emacs.d/backups"))))
(setq auto-save-file-name-transforms
     `((".*" ,(expand-file-name "~/emacs.d/backups") t)))

(fset 'gui-diff-last-failure
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 97 99 116 117 97 108 58 13 134217734 19 40 61 13 right 201326624 201326624 134217847 134217790 40 103 117 105 45 100 105 102 102 32 25 41] 0 "%d")) arg)))

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; CSS3/SCSS

(load "~/.emacs.d/scss-mode.el")
(setq css-indent-offset 4)

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

(defun foobar (msg)
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
