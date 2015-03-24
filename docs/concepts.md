## Concepts 

### Moving around

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

Then add this in your init.el.d


```elisp
(when (memq window-system '(mac ns))
     (exec-path-from-shell-initialize))
```
	 
### Buffers

You don't have windows in emacs. Everything is a *buffer* and they stack on top of each other. Buffers have
regions and they store text. All operations (functions) access text and code on buffers.

Every buffer possesses ONE major mode that determines the behavior of editing in that buffer. A row at
the bottom of the buffer called the mode line indicates the major mode. Minor modes are optional, and
there can be anywhere from 0 to a lot active at one time in a given buffer.

### Mini Buffer

The command line on the bottom of Emacs. This is where you type your Emacs commands.

### Commands

Commands are dispatched from the user to the buffer.

### Modes

Two modes: Major and Minor. You can think of Major mode as a language-specific mode. Only one major mode
is allowed at any point in time. Serveral Minor modes, on the other hand, can be active in a single Major
mode.

### nRepl

Most Clojure IDEs use a [network REPL](https://github.com/clojure/tools.nrepl) to connect to clojure
projects. This ensures seamless interaction with local devs and remote devs. With Cider, you can simply
open any .clj file from an existing leiningen project and run `M-x cider-jack-in`.

An nrepl client, configured to the localhost and port, opens up. Now you're good to code.


### Keybindings

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
		

