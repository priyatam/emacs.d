# Cider Light

A gentle introduction to Emacs using [Cider](https://github.com/clojure-emacs/cider), optimized for web development
in Clojure, Clojurescript, Javascript, Sass, and HTML5.

## Installation

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
	  
## Introduction

Emacs is one of the world's oldest and most powerful editors. However, due to its poor user-interface,
and [cryptic](http://www.emacswiki.org) docs, developers new to Lisp choose Lighttable, Sublime, or
[Cursive](https://cursiveclojure.com). In this guide I will document my struggles to learn Emacs
and port my workflow from Lightable to Cider. 

◊

In order to move around in Emacs, you must first understand these two acronyms.

   M-x (Meta x) is Opt x
   C-x (Control x) is Ctrl x

Typing `M-x` opens a command palette above the status bar. This is where you run custom commands from
changing font to installing packages, from running your custom lisp code and keyboard shortcuts
and many, many more.

Let's try to install a package. 

First, set the paths from shell (or else Emacs OS X won’t understand):

   M-x packge-install
   <RET>
   exec-path-from-shell

Then add this in your init.el.

```elisp
(when (memq window-system '(mac ns))
     (exec-path-from-shell-initialize))
```

## Packages

List available packages:

     M-x package-list-packages

To manually install a package, move the point to line number of the package and press 'i' for 'install'.
After selection, press 'x' (eXecute) to install. If you know what you want, you can also type C-f to
find the package by _first_ characters.

For deleting packages, type 'd' followed by 'x'
	
Install package:

    M-x package-install
    RET
    cider

After successfully downloading a required packag Emacs will display `done` in the status bar.

This repo's init.el config comes pre-installed with the following packages:

     cider
     magit
     js2-mode
     web-mode
     scss-mode
     projectile
     git-gutter
     auto-complete
     rainbow-delimeters
     ac-js2
    
Optionally, add support for themes by installing
[color-theme](http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip)

Without further ado, copy and paste [init.el](/init.el) from this directory into your local
`~/.emacs.d`. Once you're familiar with emacs, you can override your config file at will.

## Concepts 

**Buffers**

You don't have windows in emacs. Everything is a *buffer*. They stack on top of each other. Buffers have
regions and they store text. All operations (functions) access text and code on buffers.

Every buffer possesses ONE major mode that determines the behavior of editing in that buffer. A row at
the bottom of the buffer called the mode line indicates the major mode. Minor modes are optional, and
there can be anywhere from 0 to a lot active at one time in a given buffer.

**nRepl**

Most Clojure IDEs use a [network REPL](https://github.com/clojure/tools.nrepl) to connect to clojure
projects. This ensures seamless interaction with local devs and remote devs. With Cider, you can simply
open any .clj file from an existing leiningen project and run `M-x cider-jack-in`.

An nrepl client, configured to the localhost and port, opens up. Now you're good to code.

**Commands**

Commands are dispatched from the user to the buffer.

**Modes**

Two modes: Major and Minor. You can think of Major mode as a language-specific mode. Only one major mode
is allowed at any point in time. Serveral Minor modes, on the other hand, can be active in a single Major
mode.

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
    	
Set a bookmark

    C-x r m
    
List bookmarls
    
    C-x r l
        	
Quit command mode

    C-g

Quit Emacs

     C-x C-c
	
Move forward one screen

     C-v	
	
Move backward one screen
	
    M-v
	
Center screen with cursor position

    C-l

### Files
	
Open a browser:

     	
	
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

Open a directory tree

    C-x-d

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

    <SHFT>-P Shft P
    
### Editing

Add Comment

    Go the beginning of line and,
    M-;
    
Mark

    C-<SPC>

Copy

    C-c

Cut

    C-w

Paste

    C-y
    
Undo

    C-/
    
Remove extra space

    M-<SPC>
   
### Paredit

Slurp - Grab the thing on the right

    C-->

Unslurp - Push it right outsde your parens

    C-<-
                
Wrap parens: place cursor before the symbol
   
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
	
## References

- [Clojure with Emacs](http://clojure-doc.org/articles/tutorials/emacs.html)
- [Emacs Rocks](http://emacsrocks.com)
- [Emacs cheatsheet](http://www.rgrjr.com/emacs/emacs_cheat.html).
- [Animated guide to Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html)
- [Cider cheatsheet](https://github.com/clojure-emacs/cider#emacs-live)
