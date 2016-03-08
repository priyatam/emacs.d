(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;;(cask-initialize)

(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(s cider
	 cider-eval-sexp-fu
	 clj-refactor
	 color-theme
	 company
	 emmet-mode
	 ;; ensime
	 flycheck
	 flycheck-clojure
	 flycheck-pos-tip
	 git-timemachine
	 go-mode
	 golden-ratio
	 js2-mode
	 magit
	 markdown-mode
	 paredit
	 projectile
	 rainbow-delimiters
	 smart-mode-line
	 scss-mode
	 web-mode
	 whitespace
	 writeroom-mode
	 yaml-mode))

(require 'emmet-mode)
(require 'color-theme)
(require 'dired-x)
(require 'flycheck)
(require 'flycheck-clojure)
(require 'flycheck-pos-tip)
(require 'golden-ratio)
(require 'js)
(require 'js2-mode)
(require 'scss-mode)
(require 'web-mode)
(require 'whitespace)
(require 'writeroom-mode)
(require 'yaml-mode)

(global-prettify-symbols-mode 1)

;; Startup
(defun set-exec-path-from-shell ()
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string "[ \t\n]*$" ""
                                   (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell)
(add-to-list 'exec-path "/usr/local/bin")

;; Load modules

(load "~/.emacs.d/src/clojure.el")
(load "~/.emacs.d/src/git.el")
(load "~/.emacs.d/src/markdown.el")

;; Smart line
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; (setq default-directory "~/github/")

;; Fonts
(when (window-system)
  (set-default-font "Fira Code Light 16"))

(setq-default line-spacing 3)

(let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
               (35 . ".\\(?:[(?[_{]\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               ;;(46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (58 . ".\\(?:[:=]\\)")
               (59 . ".\\(?:;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:[:=?]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:[=@~-]\\)")
			   )
			 ))
  (dolist (char-regexp alist)
	(set-char-table-range composition-function-table (car char-regexp)
						  `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Tabs
(setq-default tab-width 4)

;; GUI
(setq inhibit-startup-message t)
(toggle-frame-fullscreen)
(setq-default fill-column 80)
(setq column-number-mode t)

;; turn off mouse interface early in startup to avoid momentary display
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (if (fboundp 'fringe-mode) (fringe-mode 0))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq cursor-type 'bar)
(set-cursor-color "#ffffff")

;;(global-linum-mode)

(global-set-key (kbd "C-x C-l") 'global-linum-mode)

;;(setq golden-ratio-auto-scale t)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/")

;; Sounds
(setq ring-bell-function 'ignore)

;; Files
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq dired-omit-files "^\\...+$")

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

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

;; Writeroom

(add-hook 'writeroom-mode
   (define-key writeroom-mode-map (kbd "s-?") nil)
   (define-key writeroom-mode-map (kbd "C-c w") #'writeroom-toggle-mode-line))

(setq writeroom-width 130)
(global-set-key (kbd "C-x C-w") 'writeroom-mode)
(global-writeroom-mode)
(setq writeroom-major-modes '(text-mode clojure-mode clojurescript-mode))


;; Scala

;; (push "/usr/local/bin/scala" exec-path)
;; (push "/usr/local/bin/sbt" exec-path)

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; BNF
(load-file "~/.emacs.d/lib/bnf-mode.el")
(add-to-list 'auto-mode-alist '("\\.bnf" . bnf-mode))

;; Javascript

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; setup paredit for js
(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; CSS3/SCSS
(setq scss-sass-command "node-sass")
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq-default scss-compile-at-save nil)

;; HTML/Templates
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

;; YAML
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Emmet
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'html-mode-hook  'emmet-mode)

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(setq emmet-move-cursor-between-quotes t)


;; KEY BINDINGS
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

(add-hook 'swift-mode-hook
          (lambda ()
			(local-set-key  (kbd "C-c M-j") 'swift-mode-run-repl)
			(local-set-key  (kbd "C-x C-e") 'swift-mode-send-region)
			(local-set-key  (kbd "C-c C-k") 'swift-mode-send-buffer)))

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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (gotham)))
 '(custom-safe-themes
   (quote
	("e3a57f02544b04d3cf6805b53fc1a94ca7c745de1317aff0d2fcc7b01ddd99d1" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fci-rule-color "#d6d6d6")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(jsx-indent-level 2)
 '(jsx-use-flymake t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#dc322f")
	 (40 . "#cb4b16")
	 (60 . "#b58900")
	 (80 . "#859900")
	 (100 . "#2aa198")
	 (120 . "#268bd2")
	 (140 . "#d33682")
	 (160 . "#6c71c4")
	 (180 . "#dc322f")
	 (200 . "#cb4b16")
	 (220 . "#b58900")
	 (240 . "#859900")
	 (260 . "#2aa198")
	 (280 . "#268bd2")
	 (300 . "#d33682")
	 (320 . "#6c71c4")
	 (340 . "#dc322f")
	 (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
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
