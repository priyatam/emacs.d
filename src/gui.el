
;; Resets

(toggle-frame-fullscreen)

(setq column-number-mode t)
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; Turn off mouse interface early in startup to avoid momentary display
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
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

;; Navigation -----

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
