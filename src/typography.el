(require 'writeroom-mode)

;; Smart line -----

(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Fonts -----

(global-prettify-symbols-mode 1)

(when (window-system)
  (set-default-font "Fira Code Light 15"))

(setq-default line-spacing 4)

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


;; Writeroom -----

(add-hook 'writeroom-mode
   (define-key writeroom-mode-map (kbd "s-?") nil)
   (define-key writeroom-mode-map (kbd "C-c w") #'writeroom-toggle-mode-line))

(setq writeroom-width 150)
(global-set-key (kbd "C-x C-w") 'writeroom-mode)
(global-writeroom-mode)
(setq writeroom-major-modes '(text-mode clojure-mode clojurescript-mode))
