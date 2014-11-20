# Cider House

A gentle introduction to Emacs [Cider](https://github.com/clojure-emacs/cider), for Lighttable Users.

## Installation

For detailed instructions, see CIDER's [README](https://github.com/clojure-emacs/cider/blob/master/README.md).

For brew lovers, install the latest version of Emacs and symlink Emacs.app to your ~/Applications folder with this:

	brew install emacs --cocoa --srgb
	brew linkapps Emacs

**Marmalade** is the package manager, built on top of package.el (an older package manager, pre emacs-23). It uses `MELPA`, a standard repo (package archives) for emacs lisp packages.

Add this as a package archive source in ~/.emacs.d/init.el:

    (require 'package)
    (add-to-list 'package-archives
        '("marmalade" . "http://marmalade-repo.org/	packages/") t)
	  (package-initialize)
	  
## Introduction

Emacs is one of the world's oldest and most powerful editors. 

However, due to its poor user-interface, and [cryptic](http://www.emacswiki.org) documentation, developers eager to learn Clojure choose Sublime, Lighttable, or Intellij. In this guide, I will document my struggles to learn Emacs and port my workflow from Lightable/Vim. 

This guide is written for OSX developers.

◊

In order to do anything in Emacs, you must first understand these two acronyms.

- M-x (Meta x) is Opt x
- C-x is Ctrl x

Great. 

Open Emacs app.

Typing `M-x` opens a command palette above the status bar. This is where you run custom commands from changing font to installing packages, and your own custom lisp code and shortcuts.

Let's try to install a package. 

Set the paths from shell (or else Emacs OS X won’t understand):

	M-x packge-install
	_hit RETURN_
	exec-path-from-shell

Then add this in your init.el.

```elisp
(when (memq window-system '(mac ns))
	(exec-path-from-shell-initialize))
```

For Clojure development, choose Cider:

	M-x package-install
	_hit RETURN_
	cider

Emacs will download the required packages from Marmalade and you should see `done` in the status bar.

Optionally, change themes:

	M-x package-install
	solarized
	
Optionally, add a file browser:	

	M-x package-install
	neotree		
	
For additional configuration, copy and paste [init.el](/init.el) into `~/.emacs.d` from this directory. You can override your init.el. Don't worry about what's inside—you're going to learn about writing your own later.  	  

## Concepts

**Buffers**

You don't have windows in emacs. Everything is a *buffer*.

They get added on top of each other. 

You can find current buffers from the 'buffers' menu.

**NREPL**

Most Clojure IDEs use a [network REPL](https://github.com/clojure/tools.nrepl) to connect to clojure projects. This ensures seamless interaction with local devs and remote devs. With Cider, you can simply open any .clj file from an existing leiningen project and run `M-x cider-jack-in`.

An nrepl client, configured to the localhost and port, opens up. 

You're good to go.

**Commands**

With Emacs on Mac OS X, you don't have to learn all the shortcuts right away, since the toolbar provides hints. If you prefer the mouse, feel free to use it.

You do, however, need to master the M-x, C-x set of primitive shortcuts.

## Commands 

### Basics

Assuming you want to learn how to open a file, move around the window (with a mouse or keyboard), install plugins, run commands, split windows, change themes/fonts etc., and evaluate clojure code out of the box ...

C-x 0 close current window
C-x 1 close all but current
C-x 2 open new BELOW
C-x 3 open new ALONGSIDE

Or, in other words ...

Remove current buffer:

	C-x-0

Back to single buffer:

	C-x-1
	
Split 'buffers’ horizontally:

	C-x-2

Split 'buffers’ vertically:

	C-x-3	

Kill current buffer by name (if you have a super memory)

	C-x-k buffname
	M-x kill-some-buffers

Move across buffers: 

	C-x-o

Increase font size:

	C-x + (hold and repeat)

Decrease font size

	C-x - (hold and repeat)

Open a file:

	C-x C-f

Save file: 

	Cmd-X
	
Fullscreen

	M-x toggle-frame-fullscreen	

Quit Emacs

	C-x C-c
	
If you've got this far, you're now able to move around.

### Eval/Repl

Open a browser:

	Fn F1
	
Enter the path of the folder to open

	~/Dev/github/user/clojureapp	

Start Cider:

	Fn F2
	
I prefer splitting my screen with a results buffer. So let’s open a buffer on the bottom with `C-x 3`.

Evaluate form and show value in buffer:

	C-x x

Evaluate form and show value in echo:

	C-c z
	
Run current file

	C-c C-k

I also prefer to open a third buffer to run code in repl.
	
	C-c C-z

## Packages

List available packages:

	M-x package-list-packages

To manually install a package, move the point to line of the package with the keyboard and press 'i' for 'install'. After selecting your favorite packages, press 'x' for 'eXecute' to install.

If you know what you want, you can also hit Cmd-f to find the package by first chars.

There are tons of packages for emacs. Don't waste your time on them right now. You're going to find your way eventually. The basic cider package lets you run any lein clojure project as a nRepl client. You get things like inline docs, paredit, and much more.

Recommended Packages:

- auto-complete
- paredit
- rainbow-delimeters
- git-gutter
- [neotree](http://www.emacswiki.org/emacs/NeoTree)

## Keybindings

There are two kinds of bindings: global and local.

For instance, my config looks like this:

	;; Key Bindings
	(global-set-key [f1] 'neotree-dir)
	(global-set-key [f2] 'cider-jack-in)
	(global-set-key [f3] 'cider-switch-to-repl-buffer)
	(global-set-key [f6] 'paredit-mode)

	(require 'bind-key)
	(bind-key "C-x z" 'cider-eval-last-sexp)
	(bind-key "C-x x" 'cider-pprint-eval-defun-at-point)

As you can see I set f1, f2, f3 to keybindings to the actual commands in Cider's [source](https://github.com/clojure-emacs/cider/blob/master/cider-mode.el), which are just symbols that execute a body of code. You're free to change these, and add personal bindings to 50+ cider commands. 

## References

- Beginner's guide to programming [Clojure with Emacs](http://clojure-doc.org/articles/tutorials/emacs.html)
- [Emacs cheatsheet](http://www.rgrjr.com/emacs/emacs_cheat.html).
- [Cider cheatsheet](https://github.com/clojure-emacs/cider#emacs-live)
- [Emacs Rocks](http://emacsrocks.com)