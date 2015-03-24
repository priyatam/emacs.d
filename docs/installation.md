## Installation

Emacs is one of the world's oldest and most powerful editors. However, due to its poor user-interface
and [cryptic](http://www.emacswiki.org) docs, developers new to Lisp choose Lighttable, Sublime, or
[Cursive](https://cursiveclojure.com). In this guide I will document my struggles to learn Emacs
and port my workflow from Sublime/Lightable to Emacs.

Install the latest version of Emacs and symlink Emacs.app to `~/Applications`:

    brew install emacs --cocoa --srgb
    brew linkapps Emacs

**Marmalade** is the package manager built on top of package.el (an older package manager pre emacs-23).
It uses `MELPA`, a standard repo (package archives) for emacs lisp packages.

First, create a file under ~/.emacs.d/init.el.

Next, add this as a package archive source in:

```elisp
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)
```

## Packages

List available packages:

     M-x package-list-packages

To install a package, move the pointer to the line number of the package and press 'i' to select.
Press 'x' (eXecute) to install. If you know what you want, you can also type C-s to
find the package by _first_ characters. For deleting packages, type 'd' followed by 'x'
	
Or you can install package directly from the mini buffer:

    M-x package-install
    RET
    cider

After successfully downloading a required package Emacs will display `done` in the status bar.

This repo's `init.el` config comes pre-installed with the following core packages:

	cider
	paredit
	company
    rainbow-delimeters
	magit
    js2-mode
    web-mode
    scss-mode
	markdown-mode
    projectile
    git-gutter
    
Optionally, add support for themes by installing [color-theme](http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip)

Without further ado, copy and paste [init.el](/init.el) from this directory into your local
`~/.emacs.d`. Once you're familiar with emacs, you can override your config file at will.
