## Cheatsheet

### Buffers

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

### Movement

Move one Character forward/backward

	C-f
	C-b

Move one Word forward/backward

	M-f
	M-b

Move one Line forward/backword

	C-n
	C-p

Move one Sentence forward/backword

	M-e
	M-a

Move one page forward/backward

	C-v
	M-v
	
Center screen with cursor position

	C-l

End of buffer

	M->
	M-<
	
Set a bookmark

    C-x r m
    
List bookmarls
    
    C-x r l
        	
Quit command mode

    C-g

Quit Emacs

    C-x C-c

### Files
	
Open a visual browser (not recommended):

	neotree
	
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
    
### Editing
	
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

Add Comment

    (Go the beginning of line)
    M-;
	
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

### Eval

### Clojure

Start Cider:

    M-x cider-connect

Switc to REPL view:

	C-c C-z

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

Set column width

	(setq column-number-mode t)
	M-Q

## Customizations

You want to customize js2-mode or web-mode, but have no about the variables. Use this trick:

	M-x customize-group [RET] <enter-mode> [RET]

Fullscreen

    M-x toggle-frame-fullscreen	
    
Increase font size:

    C-x + (hold and repeat)

Decrease font size

	C-x - (hold and repeat)

## Emmet

Expand any css-like [abbreviations](https://github.com/smihica/emmet-mode)

	C-j

Write 15 ul/li/img tags with expanded attributes in a single line:

	ul.gallery>li.gallery__item$*15>img[src="http://www.placehold.it/300x300"]

## Web Mode

Toggle Code folding

	C-c C-f

Jump to closing tag

	C-c C-n

Toggle comments

	M-;

Select tag block (say, an entire <header>)

	C-c C-m
	


