Dot Emacs
=========

My Emacs using config, optimized for web development in Clojure, Clojurescript, Javascript, HTML5, and CSS3/Less/Sass.

## Installation

Install the latest version of Emacs and symlink Emacs.app to `~/Applications`:

    brew install emacs --cocoa --srgb
    brew linkapps Emacs

**Marmalade** is the package manager built on top of package.el (an older package manager pre emacs-23).
It uses `MELPA`, a standard repo (package archives) for emacs lisp packages.

Create a file under ~/.emacs.d/init.el, and add this as a package archive source in:

```elisp
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)
```

## Packages

My Emacs config comes pre-installed with the following core packages:

	cider
	paredit
	company
    rainbow-delimeters
	magit
    js2-mode
    web-mode
    scss-mode
	emmet-mode
	markdown-mode
    projectile
    git-gutter
    
Support for themes comes through [color-theme](http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip)

## Documentation

A working guide and detailed documentation is under `docs`.
