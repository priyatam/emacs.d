# Cider Light

A lightweight Emacs Cider configuration, with a gentle introduction to Emacs Cider[Cider](https://github.com/clojure-emacs/cider).

## Installation

This guide is optimized for OSX.

Install the latest version of Emacs and symlink Emacs.app to `~/Applications`:

	brew install emacs --cocoa --srgb
	brew linkapps Emacs

For detailed instructions, see CIDER's [README](https://github.com/clojure-emacs/cider/blob/master/README.md).

**Marmalade** is the package manager built on top of package.el (an older package manager pre emacs-23). It uses `MELPA`, a standard repo (package archives) for emacs lisp packages.

First, create a file under ~/.emacs.d/init.el.

Next, add this as a package archive source in:

```elisp
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)
```
	  
## Introduction

Emacs is one of the world's oldest and most powerful editors. 

However, due to its poor user-interface, and [cryptic](http://www.emacswiki.org) docs, developers new to Clojure choose Sublime, Lighttable, or [Cursive](https://cursiveclojure.com). In this guide I will document my struggles to learn Emacs and port my workflow from Lightable to Cider. 

◊

In order to do anything in Emacs, you must first understand these two acronyms.

	M-x (Meta x) is Opt x
	C-x is Ctrl x

Open Emacs app.

Typing `M-x` opens a command palette above the status bar. This is where you run custom commands from changing font to installing packages, from running your custom lisp code and keyboard shortcuts and many, many more.

Let's try to install a package. 

First, set the paths from shell (or else Emacs OS X won’t understand):

	M-x packge-install
	_RETURN_
	exec-path-from-shell

Then add this in your init.el.

```elisp
(when (memq window-system '(mac ns))
	(exec-path-from-shell-initialize))
```

Install Cider:

	M-x package-install
	cider

Emacs will download the required packages from Marmalade and you should see `done` in the status bar.

Optionally, change themes:

	M-x package-install
	solarized
	
Add a file browser:	

	M-x package-install
	neotree		
	
For additional configuration, copy and paste [init.el](/init.el)  from this directory into your local `~/.emacs.d`. You can override your init.el. Don't worry about what's inside—you're going to learn about writing your own customizations later. 	 
## Concepts

**Buffers**

You don't have windows in emacs. Everything is a *buffer*. They get added on top of each other. Buffers have regions and they store text. All operations (functions) access text and code on buffers. 

Every buffer possesses ONE major mode that determines the behavior of editing in that buffer. A row at the bottom of the buffer called the mode line indicates the major mode. Minor modes are optional, and there can be anywhere from 0 to a lot active at one time in a given buffer.

**nRepl**

Most Clojure IDEs use a [network REPL](https://github.com/clojure/tools.nrepl) to connect to clojure projects. This ensures seamless interaction with local devs and remote devs. With Cider, you can simply open any .clj file from an existing leiningen project and run `M-x cider-jack-in`.

An nrepl client, configured to the localhost and port, opens up. Now you're good to code.

**Commands**

Commands are dispatched from the user to the buffer. Based on what

With Emacs on Mac OS X, you don't have to learn all the shortcuts right away. Feel free to use the mouse. 

## Commands 

### Moving around

Create/Switch buffers:	

	C-x 0 close current window 
	C-x 1 close all but current
	C-x 2 open new BELOW / split horizontally
	C-x 3 open new ALONGSIDE / split vertically

Kill current buffer by name

	C-x-k buffname

Move across buffers: 

	C-x-o
	
Previous/Next Buffer states:

    C-c ->
    C-c <-	        
    	
Quit command mode

    C-g	    

Quit Emacs

	C-x C-c

### Files
	
Open a browser:

	Fn-F1	
	
Open a file:

	C-x C-f

Save file: 

	C-x C-e
	Cmd-X
	
Goto line
    
    M-g-g  	  
    
Goto start/end of line

    C-A
    C-K
    C-E  

Goto start/end of page

    S-M-<
    S-M->

Find forward/backward  
    
    C-s
    C-r
    
Replace

    M-x replace-string
                
Format code

    C-c-n             

### Eval

Start Cider:

    M-x cider-connect

Start a dedicated repl:

    Fn-F3

Evaluate form and show value in status bar:

    C-c C-e
	
Evaluate current file

    C-c C-k	
    
Jumpt into symbol

    M-.
    M-,
    
Move to next sexpr

    C-M-f
    
Move to last sexpr

    C-M-b                   	
	
### Git
	
Maggit status

    C-c g        

Inspect files

    move up/down and tab into filename to see changes
    
Stash/Unstash

    s/u
    
Commit

    c c
    
Push

    Shft P Shft P           
    

### Editing

Add Comment

    Go the beginning of line and,
    M-;
    
### Paredit


Slurp - Grab the thing on the right

    C-->    

Unslurp - Push it right outsde your parens

    C-<-
                
Wrap parens: place cursor before   
   
    M-(
   
    
### Misc

Open a shell:

   M-x eshell

Fullscreen

	M-x toggle-frame-fullscreen	
    
Increase font size:

	C-x + (hold and repeat)

Decrease font size

	C-x - (hold and repeat)                
    	
## Packages

List available packages:

	M-x package-list-packages

To manually install a package, move the point to line number of the package and press 'i' for 'install'. After selection, press 'x' (eXecute) to install. If you know what you want, you can also type Cmd-f to find the package by _first_ characters.

_Note to self_: there are tons of packages for Emacs. Don't waste your time on them right now. You're going to find your way eventually. The default cider IDE comes pre-loaded with most of what you require anyway. You get things like inline docs, paredit, and much more.

Recommended Packages:

- auto-complete
- magit
- [neotree](http://www.emacswiki.org/emacs/NeoTree)
- paredit
- projectile 
- rainbow-delimeters

## References

- Beginner's guide to programming [Clojure with Emacs](http://clojure-doc.org/articles/tutorials/emacs.html)
- [Emacs cheatsheet](http://www.rgrjr.com/emacs/emacs_cheat.html).
- [Cider cheatsheet](https://github.com/clojure-emacs/cider#emacs-live)
- [Emacs Rocks](http://emacsrocks.com)