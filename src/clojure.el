;; Clojure-mode and inf-clojure

(require 'clojure-mode)
(require 'inf-clojure)

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(setq inf-clojure-repl-use-same-window nil)
(setq inf-clojure-generic-cmd '("localhost" 5555))
(setf inf-clojure-lein-cmd '("localhost" . 5555))

;; paredit
(require 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; projectile
(require 'projectile)
(add-hook 'clojure-mode-hook 'projectile-mode)

;; company-mode
(require 'company)
(require 'company-etags)
(add-to-list 'company-etags-modes 'clojure-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; syntax
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)

;; clojure specific
(add-to-list 'auto-mode-alist '("\\.boot\'" . clojure-mode))

(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'clojurescript-mode-hook (lambda () (whitespace-mode t)))
(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 80)

;; coding standards
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
  (when 'defun)
  (when-not 'defun)
  (doto 'cond)
  (deftest 'defun)
  (testing 'defun)
  (button 'defun)
  (textarea 'defun)
  (try 'defun)
  (try+ 'defun))

;; figwheel ---

(defun figwheel-repl ()
  (interactive)
  (inf-clojure "lein figwheel"))

(setq clojure-align-forms-automatically t)
