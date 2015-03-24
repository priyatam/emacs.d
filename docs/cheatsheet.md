## Cheatsheet

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

## Mode Customizations

Sometimes you want to customize js2-mode or web-mode, but have no about the variables. Use this trick:

	M-x customize-group [RET] <enter-mode> [RET]

## Emmet
