
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
;; mac (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(require 'cask "~/.cask/cask.el")

(cask-initialize)

(load "~/.emacs.d/src/common.el")
(load "~/.emacs.d/src/cider.el")
;; (load "~/.emacs.d/src/inf-clojure.el")
(load "~/.emacs.d/src/css.el")
(load "~/.emacs.d/src/elisp.el")
(load "~/.emacs.d/src/files.el")
(load "~/.emacs.d/src/git.el")
(load "~/.emacs.d/src/gui.el")
(load "~/.emacs.d/src/keybindings.el")
(load "~/.emacs.d/src/js.el")
(load "~/.emacs.d/src/markups.el")
(load "~/.emacs.d/src/typography.el")
(load "~/.emacs.d/src/utils.el")
;; (load "~/.emacs.d/src/writing.el")

(set-exec-path-from-shell)
(add-to-list 'exec-path "/usr/local/bin")

;; Temporarily reduce gc at startup
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")

(setq gc-cons-threshold (* 1024 1024 1024))

(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
	("4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "1025e775a6d93981454680ddef169b6c51cc14cea8cb02d1872f9d3ce7a1da66" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "304c39b190267e9b863c0cf9c989da76dcfbb0649cbcb89592e7c5c08348fce9" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "428bdd4b98d4d58cd094e7e074c4a82151ad4a77b9c9e30d75c56dc5a07f26c5" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "69e7e7069edb56f9ed08c28ccf0db7af8f30134cab6415d5cf38ec5967348a3c" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "b0c5c6cc59d530d3f6fbcfa67801993669ce062dda1435014f74cafac7d86246" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "732ccca2e9170bcfd4ee5070159923f0c811e52b019106b1fc5eaa043dff4030" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "b67b2279fa90e4098aa126d8356931c7a76921001ddff0a8d4a0541080dee5f6" "a61109d38200252de49997a49d84045c726fa8d0f4dd637fce0b8affaa5c8620" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "8553bdd21237bcc7d9cfabd93c1525af9ac4a84d5ab1435e6474f9fd4f81d69c" "bf25a2d5c2eddc36b2ee6fc0342201eb04ea090e637562c95b3b6e071216b524" default)))
 '(fci-rule-color "#d6d6d6")
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(jsx-indent-level 2)
 '(jsx-use-flymake t)
 '(package-selected-packages
   (quote
	(fountain-mode elm-mode solidity-mode clj-refactor cider clojure-mode-extra-font-locking base16-theme indium meghanada neotree yaml-mode writeroom-mode web-mode use-package smex smart-mode-line scss-mode rainbow-delimiters projectile prodigy pos-tip paredit pallet markdown-mode magit js2-mode inf-clojure idle-highlight-mode helm-dash go-mode git-timemachine flycheck-cask expand-region exec-path-from-shell emmet-mode drag-stuff company)))
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
