(require 'writeroom-mode)

(load "~/.emacs.d/lib/typopunct.el")
(require 'typopunct)

;; Smart line -----

(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Fonts -----

(global-prettify-symbols-mode 1)

(when (window-system)
  (set-default-font "Fira Code 12"))

(setq-default line-spacing 6)

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

(setq-default fill-column 80)
(setq writeroom-width 80)
(global-set-key (kbd "C-x C-w") 'writeroom-mode)
(global-writeroom-mode)
(setq writeroom-major-modes '(text-mode))


;; Typopunct

(defconst typopunct-ellipsis (decode-char 'ucs #x2026))
(defconst typopunct-middot   (decode-char 'ucs #x2219))
(defun typopunct-insert-ellipsis-or-middot (arg)
  "Change three consecutive dots to a typographical ellipsis mark."
  (interactive "p")
  (cond
   ((and (= 1 arg)
		 (eq (char-before) ?^))
	(delete-char -1)
	(insert typopunct-middot))
   ((and (= 1 arg)
		 (eq this-command last-command)
		 (looking-back "\\.\\."))
	(replace-match "")
	(insert typopunct-ellipsis))
   (t
	(self-insert-command arg))))

(define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)


(add-hook 'markdown-mode-hook 'init)
