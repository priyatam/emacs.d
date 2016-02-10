
;; BNF based syntax checker, version v.0042
;; By Jeppe Welling Hansen
;; u061245@cs.au.dk
;; jeppewelling@gmail.com
;; http://daimi.au.dk/~u061245/thesis



(defun bnf-mode-bnf-mode-activation-key ()
  (kbd "M-n"))

(setq max-lisp-eval-depth 1000000)
(setq max-specpdl-size 10000)
;; If the syntax checker did not accept the input, bnf-mode-false is returned
;; along with an error message.
(defun bnf-mode-false (v)
  (if (bnf-mode-is-false? v)
      (error "false value is false: %s" v)
    (list 'bnf-mode-false v)))

(defun bnf-mode-is-false? (v)
  (and
   (consp v)
   (eq (car v) 'bnf-mode-false)))

;; Assumes that v is a bnf-mode-false value.
(defun bnf-mode-false-1 (v)
  (nth 1 v))


(defun bnf-mode-true ()
  (list 'bnf-mode-true))

(defun bnf-mode-is-true? (v)
  (and 
   (consp v)
   (eq (car v) 'bnf-mode-true)))

(defun bnf-mode-boolean? (v)
  (or (bnf-mode-is-true? v)
      (bnf-mode-is-false? v)))

;; Returns the bnf-mode-first bnf-mode-false from left to right or bnf-mode-true
;; n booleans --> boolean
(defun bnf-mode-boolean-and (&rest vs)
  (bnf-mode-fold-left-break-at
   vs
   (lambda (bnf-mode-x rest)
     bnf-mode-x)
   (lambda () (bnf-mode-true))
   (lambda (bnf-mode-x rest)
     (bnf-mode-is-false? bnf-mode-x))))

(provide 'bnf-booleans)
;; The bnf-mode-trampoline and its data structures.

(defun bnf-mode-trampoline-stop (v)
  (vector 'bnf-mode-trampoline-stop v))

(defun bnf-mode-is-trampoline-stop? (v)
  (bnf-mode-array-base-predicate v 2 'bnf-mode-trampoline-stop))

(defun bnf-mode-trampoline-stop-1 (v)
  (aref v 1))


(defun bnf-mode-trampoline-continue (v)
  (vector 'bnf-mode-trampoline-continue v))

(defun bnf-mode-is-trampoline-continue? (v)
  (bnf-mode-array-base-predicate v 2 'bnf-mode-trampoline-continue))

(defun bnf-mode-trampoline-continue-1 (v)
  (aref v 1))




;; A generic bnf-mode-trampoline function, currently not in use.
;; (defun bnf-mode-trampoline (v)
;;   (cond

;;    ((bnf-mode-is-trampoline-stop? v)
;;     (bnf-mode-trampoline-stop-1 v))

;;    ((bnf-mode-is-trampoline-continue? v)
;;     (setq res (funcall (bnf-mode-trampoline-continue-1 v)))
;;     (while (bnf-mode-is-trampoline-continue? res)
;;       (setq res (funcall (bnf-mode-trampoline-continue-1 res))))
;;     (bnf-mode-trampoline-stop-1 res))))

(provide 'bnf-mode-trampoline)
;; The group name without < >
(defun bnf-mode-strip-arrows (group-name)
  (make-symbol
   (replace-regexp-in-string 
    "[<>]" ""
    (symbol-name group-name))))

;; string helpers
(defun bnf-mode-trim-string (string)
    "Removes starting and trainling whitespace."
    (if (null string)
	nil
      (replace-regexp-in-string 
       "\\`[ \t\n]*" "" 
       (replace-regexp-in-string "[ \t\n]*\\'" "" string))))

(provide 'string-functions);; String predicates

(defun bnf-mode-is-str-comma? (v)
  (string= "," v))

(defun bnf-mode-is-str-backward-quote? (v)
  (string= "`" v))


(defun bnf-mode-is-str-comment? (v)
  (string= ";" v))

(defun bnf-mode-is-str-quote? (v)
  (string= "'" v))

(defun bnf-mode-is-str-double-quote? (v)
  (string= "\"" v))


(defun bnf-mode-is-str-white-space? (v)
  (or (string= " " v)
      (string= "\t" v)
      (bnf-mode-is-str-new-line? v)))

(defun bnf-mode-is-str-new-line? (v)
  (string= "\n" v))

(defun bnf-mode-is-str-arrow-begin? (v)
  (string= "<" v))

(defun bnf-mode-is-str-arrow-end? (v)
  (string= ">" v))

(defun bnf-mode-is-str-member? (v)
  (string= "|" v))

;; If the token is non of the other elemnts, it is treated as a string
;; constant.
;; http://emacswiki.org/emacs/RegularExpression
(defun bnf-mode-is-str-constant? (v)
  (and 
    (not (= (length v) 0))
    (let ((i (string-match "^[^])}[{(\s\n\t]+$" v)))
      (if i
	  (= 0 i)
	i))))




;; For the bnf parsing < and > are not constants
(defun bnf-mode-is-str-bnf-constant? (v)
  (and 
    (not (= (length v) 0))
    (let ((i (string-match "^[^])}[{(\s\n\t><\+*]+$" v)))
      (if i
	  (= 0 i)
	i))))


(defun bnf-mode-is-str-star? (v)
  (string= v "*"))

(defun bnf-mode-is-str-plus? (v)
  (string= v "+"))

;; helper for paren
(defun bnf-mode-is-str-paren-p-v? (paren v)
  (string=  v paren))

(defun bnf-mode-is-str-paren-begin-curl? (v)
  (bnf-mode-is-str-paren-p-v? "{" v))

(defun bnf-mode-is-str-paren-begin-c? (v)
  (bnf-mode-is-str-paren-p-v? "(" v))

(defun bnf-mode-is-str-paren-begin-sqr? (v)
  (bnf-mode-is-str-paren-p-v? "[" v))

(defun bnf-mode-is-str-paren-end-curl? (v)
  (bnf-mode-is-str-paren-p-v? "}" v))

(defun bnf-mode-is-str-paren-end-c? (v)
  (bnf-mode-is-str-paren-p-v? ")" v))

(defun bnf-mode-is-str-paren-end-sqr? (v)
  (bnf-mode-is-str-paren-p-v? "]" v))


(defun bnf-mode-is-str-paren-begin? (v)
  (or (bnf-mode-is-str-paren-begin-curl? v)
      (bnf-mode-is-str-paren-begin-c? v)
      (bnf-mode-is-str-paren-begin-sqr? v)))

;; (bnf-mode-is-str-paren-begin? "{")

(defun bnf-mode-is-str-paren-end? (v)
  (or (bnf-mode-is-str-paren-end-curl? v)
      (bnf-mode-is-str-paren-end-c? v)
      (bnf-mode-is-str-paren-end-sqr? v)))


;; str -> paren-type
(defun bnf-mode-derive-paren-type (str)
  (cond 
   ((string= str "{") (bnf-mode-make-paren-type-curl))
   ((string= str "(") (bnf-mode-make-paren-type-c))
   ((string= str "[") (bnf-mode-make-paren-type-sqr))
   ((string= str "}") (bnf-mode-make-paren-type-curl))
   ((string= str ")") (bnf-mode-make-paren-type-c))
   ((string= str "]") (bnf-mode-make-paren-type-sqr))))


(defun bnf-mode-is-str-string? (v)
  (if (or (not (string= "\"" (car v))) 
	  (null v))
      nil
    (let* ((saw-quote nil) (r (bnf-mode-fold-left-break-after
	       (cdr v)
	       (lambda (bnf-mode-x rest)
		 (if (bnf-mode-is-str-double-quote? bnf-mode-x)
		     (setq saw-quote t))
		  
		 (bnf-mode-make-parse-remainder
		  (concat (bnf-mode-parse-remainder-1 rest) bnf-mode-x)
		  (cdr (bnf-mode-parse-remainder-2 rest))
		  (+ 1 (bnf-mode-parse-remainder-3 rest))))

	       (lambda () (bnf-mode-make-parse-remainder
			   (car v)
			   (cdr v)
			   1))
	       (lambda (bnf-mode-x rest) 
		 (bnf-mode-is-str-double-quote? bnf-mode-x))
	       )))

      (cond

       ((not saw-quote)
	nil)

       ((= 0 (length (bnf-mode-parse-remainder-1 r)))
	nil)
       
       (t
	r)))))

(provide 'string-predicates)


;; A set join
(defun bnf-mode-set-join (set word)
  (if (member word set)
      set
    (append set (list word))))


;; A set append
(defun bnf-mode-set-append (left-set right)
  (bnf-mode-fold-left
   right
   (lambda (bnf-mode-x rest)
     (bnf-mode-set-join rest bnf-mode-x))
   (lambda () left-set)))


(defun bnf-mode-file-string (file)
    "Read the contents of a file and return as a string."
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))

;; move to util file
(defun bnf-mode-n-first-of-list (n v)
  (bnf-mode-fold-left-position-nr-break
   v
   (lambda (bnf-mode-x i rest)
     (append
      rest
      (list bnf-mode-x)))
   (lambda () '())
   (lambda (bnf-mode-x i rest)
     (= n i))))

;; (bnf-mode-n-first-of-list 1 '(1 2))



;; move to util file
(defun bnf-mode-n-last-of-list (n v)
  (bnf-mode-fold-right-position-nr-break
   v
   (lambda (bnf-mode-x i rest)
     (cons bnf-mode-x rest))
   (lambda () '())
   (lambda (bnf-mode-x i rest)
     (= n i))))

 ;; (bnf-mode-n-last-of-list 3 '(1 2 3))
 ;; (bnf-mode-n-last-of-list 0 '(1 2 3))

;; move to util file
(defun bnf-mode-remove-first-n-last-m (n m v)
  (let ((first-removed (nthcdr n v)))
  (bnf-mode-n-first-of-list 
   (- (length first-removed) m)
   first-removed)))


(defun bnf-mode-bnf-mode-string-to-list (str)
  (split-string str "" t))

(defun bnf-mode-list-to-string (xs)
  (bnf-mode-list-to-string-visit xs ""))

(defun bnf-mode-list-to-string-visit (xs str)
  (if xs
      (bnf-mode-list-to-string-visit
       (cdr xs)
       (concat str (car xs)))
    str))




;; f * str * list -> str
(defun bnf-mode-mapstr (f str list)
  "Apples the function, f, to each element of the list along with
str while passing the previous str to the next function
application."
  (if list
    (bnf-mode-mapstr
     f 
     (funcall f (car list) str) 
     (cdr list))
    str))

;; A library for recursive paradigmes in a none recursive world


;; A none recursive bnf-mode-map-list
;; O(n)
(defun bnf-mode-map-list (lst f-ind)
  (let ((m '()))
    (while lst
      (setq m (append m (list (funcall f-ind (car lst)))))
      (setq lst (cdr lst)))
    m))
;(bnf-mode-map-list '(1 2 3 4 5 6) (lambda (bnf-mode-x) (+ 1 bnf-mode-x)))

;; A none recursive bnf-mode-fold-right for lists
;; O(2n)
(defun bnf-mode-fold-right (lst f-ind f-null)
  (let ((m '()) (res '()))
    (while lst
      (setq m (cons (car lst) m))
      (setq lst (cdr lst)))
    (setq res (funcall f-null))
    (while m
      (setq res (funcall f-ind (car m) res))
      (setq m (cdr m)))
    res))

;; (fold-list '(0 1 2 3) (lambda (bnf-mode-x rest) (cons bnf-mode-x rest)) (lambda () '()))


(defun bnf-mode-fold-right-break-before (lst f-ind f-null f-break)
  (let ((m '()) (res '()))
    (while lst
      (setq m (cons (car lst) m))
      (setq lst (cdr lst)))
    (setq res (funcall f-null))
    (while m
      (if (funcall f-break (car m) res)
	  (setq m nil)
	(progn
	  (setq res (funcall f-ind (car m) res))
	  (setq m (cdr m)))))
    res))

(defun bnf-mode-fold-right-break-at (lst f-ind f-null f-break)
  (let ((m '()) (res '()))
    (while lst
      (setq m (cons (car lst) m))
      (setq lst (cdr lst)))
    (setq res (funcall f-null))
    (while m
      (if (funcall f-break (car m) res)
	  (progn
	    (setq res (funcall f-ind (car m) res))
	    (setq m nil))
	(progn
	  (setq res (funcall f-ind (car m) res))
	  (setq m (cdr m)))))
    res))



(defun bnf-mode-fold-right-position-nr-break (lst f-ind f-null f-break)
  (let ((res nil) (i 0) (m '()))
    (while lst
      (setq m (cons (car lst) m))
      (setq lst (cdr lst)))
    (setq res (funcall f-null))
    (while m
      (if (funcall f-break (car m) i res)
	  (setq m nil)
	(progn
	  (setq res (funcall f-ind (car m) i res))
	  (setq i (+ i 1))
	  (setq m (cdr m)))))
    res
    ))




;; A none recursive bnf-mode-fold-left for lists
;; O(n)
(defun bnf-mode-fold-left (lst f-ind f-null)
  (let ((res nil))
    (setq res (funcall f-null))
    (while lst
      (setq res (funcall f-ind (car lst) res))
      (setq lst (cdr lst)))
    res
    ))

;; (bnf-mode-fold-left '(1 2 1 1) (lambda (bnf-mode-x rest) (+ bnf-mode-x rest)) (lambda () 0))
;; (bnf-mode-fold-left '(501 2 101 3) (lambda (bnf-mode-x rest) 
;; 			   (if (> bnf-mode-x rest) 
;; 			       bnf-mode-x 
;; 			     rest)) (lambda () 0))



;; Used to break the fold on some event defined by f-break.
(defun bnf-mode-fold-left-break-at (lst f-ind f-null f-break)
  "list f-ind f-null f-break"
  (let ((res nil))
    (setq res (funcall f-null))
    (while lst
      (if (funcall f-break (car lst) res)
	  (progn
	    (setq res (funcall f-ind (car lst) res))
	    (setq lst nil))
	(progn
	  (setq res (funcall f-ind (car lst) res))
	  (setq lst (cdr lst)))))
    res
    ))




(defun bnf-mode-fold-left-break-after (lst f-ind f-null f-break)
  "list f-ind f-null f-break"
  (let ((res nil))
    (setq res (funcall f-null))
    (while lst
      (setq res (funcall f-ind (car lst) res))
      (if (funcall f-break (car lst) res)
	  (setq lst nil)
	(setq lst (cdr lst))))
    res
    ))


;; (bnf-mode-fold-left-break-after
;;  '(1 2 3)
;;  (lambda (bnf-mode-x res) (+ bnf-mode-x res))
;;  (lambda () 0)
;;  (lambda (bnf-mode-x res) (= bnf-mode-x 2)))


(defun bnf-mode-fold-left-break-before (lst f-ind f-null f-break)
  "list f-ind f-null f-break"
  (let ((res nil))
    (setq res (funcall f-null))
    (while lst
      (if (funcall f-break (car lst) res)
	  (setq lst nil)
	(progn
	  (setq res (funcall f-ind (car lst) res))
	  (setq lst (cdr lst)))))
    res
    ))


 ;; (fold-left-break '(1 2 3 4) 
 ;; 		  (lambda (bnf-mode-x rest) bnf-mode-x)
 ;; 		  (lambda () 0)
 ;; 		  (lambda (bnf-mode-x rest) 
 ;; 		    (= 2 bnf-mode-x)))



(defun bnf-mode-fold-left-position-nr-break (lst f-ind f-null f-break)
  (let ((res nil) (i 0))
    (setq res (funcall f-null))
    (while lst
      (if (funcall f-break (car lst) i res)
	  (setq lst nil)
	(progn
	  (setq res (funcall f-ind (car lst) i res))
	  (setq i (+ i 1))
	  (setq lst (cdr lst)))))
    res
    ))

(defun bnf-mode-fold-left-position-nr (lst f-ind f-null)
  (let ((res nil) (i 0))
    (setq res (funcall f-null))
    (while lst
      (setq res (funcall f-ind (car lst) i res))
      (setq i (+ i 1))
      (setq lst (cdr lst)))
    res
    ))

(defun bnf-mode-map-list-position-nr (lst f)
  (let ((m '()) (i 0))
    (while lst
      (setq m (append m (list (funcall f (car lst) i))))
      (setq i (+ i 1))
      (setq lst (cdr lst)))
    m))

(provide 'bnf-util)
;; Helpers for the data structure
(defun bnf-mode-list-of-length (v n)
  (= n (length v)))

(defun bnf-mode-base-predicate (v n name)
  (and (bnf-mode-list-of-length v n)
       (equal (car v) name)))

(defun bnf-mode-is-constructor? (v)
  "Determines wether the value is a data constructor or not"
  (or (bnf-mode-is-bnf? v)
      (bnf-mode-is-groups? v)
      (bnf-mode-is-group? v)
      (bnf-mode-is-members? v)
      (bnf-mode-is-member? v)
      (bnf-mode-is-paren? v)
      (bnf-mode-is-constant? v)
      (bnf-mode-is-quote? v)
      (bnf-mode-is-comma? v)
      (bnf-mode-is-backward-quote? v)
      (bnf-mode-is-group-ref? v)
      (bnf-mode-is-star? v)
      (bnf-mode-is-plus? v)
      (bnf-mode-is-syntax-paren? v)
      (bnf-mode-is-syntax-quote? v )
      (bnf-mode-is-syntax-backward-quote? v )
      (bnf-mode-is-syntax-comma? v )))

(defun bnf-mode-is-value-list? (v)
  (and (not (null v))
       (not (bnf-mode-is-constructor? v))
       ;; Just check the bnf-mode-first element, assume rest is also constructor types
       (bnf-mode-is-constructor? (car v))))

(defun bnf-mode-is-syntax-value? (v)
  (and (not (null v))
       (or (bnf-mode-is-constant? v)
	   (bnf-mode-is-syntax-quote? v)
	   (bnf-mode-is-syntax-comma? v)
	   (bnf-mode-is-syntax-backward-quote? v)
	   (bnf-mode-is-syntax-paren? v))))

(defun bnf-mode-is-syntax-value-list? (v)
  (cond

   ((null v)
    nil)

   ((bnf-mode-is-syntax-value? v)
    nil)

   ;; Assume that the rest of the elements are all syntax values
   (t
    (bnf-mode-is-syntax-value? (car v)))))
  

(defun bnf-mode-first (v)
  "First value in a constructor excluding the location"
  (nth 2 v))

(bnf-mode-first 
 '(a b c))


(defun bnf-mode-second (v)
  "Second value in a constructor excluding the location"
  (nth 3 v))


(defun bnf-mode-third (v)
  "Third value in a constructor excluding the location"
  (nth 4 v))


(defun bnf-mode-fourth (v)
  "Fourth value in a constructor excluding the location"
  (nth 4 v))

;;;


(defun bnf-mode-extend-bnf-location (a b)
  (bnf-mode-make-bnf-location
   (bnf-mode-bnf-location-1 a)
   (bnf-mode-bnf-location-2 b)))

(defun bnf-mode-extend-syntax-location (a b)
  (bnf-mode-make-syntax-location
   (bnf-mode-syntax-location-1 a)
   (bnf-mode-syntax-location-2 b)))




(defun bnf-mode-bnf-location-from-error (e)
  (nth 1 e))      

(defun bnf-mode-bnf-location-from-value (v)
  (cond
   ((bnf-mode-is-constructor? v)
    (if (or (bnf-mode-is-bnf-location? (nth 1 v)) 
	    (bnf-mode-is-syntax-location? (nth 1 v)))
	(nth 1 v)
      (error "Value had no bnf location: %s" v)))

   (t
    (bnf-mode-bnf-location-from-value (car v)))))


;; The new BNF data representation
(defun bnf-mode-make-bnf-location (col-start col-end)
  (list 'bnf-location col-start col-end))

(defun bnf-mode-is-bnf-location? (v)
  (bnf-mode-base-predicate v 3 'bnf-location))

(defun bnf-mode-bnf-location-1 (v)
  (nth 1 v))

(defun bnf-mode-bnf-location-2 (v)
  (nth 2 v))

;;;

(defun bnf-mode-make-bnf (location groups)
  (list 'bnf location groups))

(defun bnf-mode-is-bnf? (v)
  (bnf-mode-base-predicate v 3 'bnf))

(defun bnf-mode-bnf-1 (v)
  (bnf-mode-first v))




;;;

(defun bnf-mode-make-groups (location groups)
  (list 'groups location groups))

(defun bnf-mode-is-groups? (v)
  (bnf-mode-base-predicate v 3 'groups))

(defun bnf-mode-groups-1 (v)
  (bnf-mode-first v))

;;;

(defun bnf-mode-make-group (location name members)
  "location name members"
  (list 'group location name members))

(defun bnf-mode-is-group? (v)
  (bnf-mode-base-predicate v 4 'group))

(defun bnf-mode-group-1 (v)
  (bnf-mode-first v))

(defun bnf-mode-group-2 (v)
  (bnf-mode-second v))

;;;

(defun bnf-mode-make-members (loc v)
  (list 'members loc v))

(defun bnf-mode-is-members? (v)
  (bnf-mode-base-predicate v 3 'members))

(defun bnf-mode-members-1 (v)
  (bnf-mode-first v))

;;;

(defun bnf-mode-make-member (loc v)
  "location value"
  (list 'member loc 
	(if (bnf-mode-is-constructor? v)
	    (list v)
	  v)))

(defun bnf-mode-is-member? (v)
  (bnf-mode-base-predicate v 3 'member))

(defun bnf-mode-member-1 (v)
  (bnf-mode-first v))

;;;

(defun bnf-mode-make-constant (loc v)
  (list 'constant loc v))

(defun bnf-mode-is-constant? (v)
  (bnf-mode-base-predicate v 3 'constant))

(defun bnf-mode-constant-1 (v)
  (bnf-mode-first v))


;;;

(defun bnf-mode-make-group-ref (loc v)
  (list 'group-ref loc v))

(defun bnf-mode-is-group-ref? (v)
  (bnf-mode-base-predicate v 3 'group-ref))

(defun bnf-mode-group-ref-1 (v)
  (bnf-mode-first v))

;;;

(defun bnf-mode-make-quote (loc content)
  (list 'quote loc content))

(defun bnf-mode-is-quote? (v)
  (bnf-mode-base-predicate v 3 'quote))

(defun bnf-mode-quote-1 (v)
  (bnf-mode-first v))
;;;

(defun bnf-mode-make-comma (loc content)
  (list 'comma loc content))

(defun bnf-mode-is-comma? (v)
  (bnf-mode-base-predicate v 3 'comma))

(defun bnf-mode-comma-1 (v)
  (bnf-mode-first v))


;;;


(defun bnf-mode-make-backward-quote (loc content)
  (list 'backward-quote loc content))

(defun bnf-mode-is-backward-quote? (v)
  (bnf-mode-base-predicate v 3 'backward-quote))

(defun bnf-mode-backward-quote-1 (v)
  (bnf-mode-first v))
;;;


(defun bnf-mode-make-paren (loc paren-type content)
  (list 'paren loc paren-type content))

(defun bnf-mode-is-paren? (v)
  (bnf-mode-base-predicate v 4 'paren))

(defun bnf-mode-paren-1 (v)
  (bnf-mode-first v))

(defun bnf-mode-paren-2 (v)
  (bnf-mode-second v))

;;;

(defun bnf-mode-make-paren-type-sqr ()
  'sqr)

(defun bnf-mode-make-paren-type-c ()
  'c)

(defun bnf-mode-make-paren-type-curl ()
  'curl)

(defun bnf-mode-is-paren-type-c? (v)
  (eq (bnf-mode-make-paren-type-c) v))

(defun bnf-mode-is-paren-type-sqr? (v)
  (eq (bnf-mode-make-paren-type-sqr) v))

(defun bnf-mode-is-paren-type-curl? (v)
  (eq (bnf-mode-make-paren-type-curl) v))

;;;

(defun bnf-mode-make-star (loc v)
  "location value"
  (list 'star loc v))

(defun bnf-mode-is-star? (v)
  (bnf-mode-base-predicate v 3 'star))

(defun bnf-mode-star-1 (v)
  (bnf-mode-first v))

;;;

(defun bnf-mode-make-plus (loc v)
  (list 'plus loc v))

(defun bnf-mode-is-plus? (v)
  (bnf-mode-base-predicate v 3 'plus))

(defun bnf-mode-plus-1 (v)
  (bnf-mode-first v))


;;;;;;;;;;;; SEXP PARSER DATA STRUCTURE ;;;;;;;;;;;;;;;;;

;; A constructor for the syntax location
(defun bnf-mode-make-syntax-location (start-column end-column)
  (list 'syntax-location start-column end-column))

(defun bnf-mode-is-syntax-location? (v)
  (bnf-mode-base-predicate v 3 'syntax-location))

(defun bnf-mode-syntax-location-1 (v)
  (nth 1 v))

(defun bnf-mode-syntax-location-2 (v)
  (nth 2 v))


(defun bnf-mode-syntax-location-from-value (v)
  (cond
   
   ((null v)
    (error "value is null"))

   ((bnf-mode-is-syntax-value? v)
    (if (bnf-mode-is-syntax-location? (nth 1 v))
	(nth 1 v)
      (error "value did not have a syntax location")))

   ;; Merge the location of the bnf-mode-first and the last value in the list
   ((bnf-mode-is-syntax-value-list? v)
    (bnf-mode-extend-syntax-location
     (bnf-mode-syntax-location-from-value (car v))
     (bnf-mode-syntax-location-from-value (bnf-mode-last-element-of-list v))))

   (t
    (error "unable to extract syntax-location from value"))))








;; Data structure for the parse remainder
(defun bnf-mode-make-parse-remainder (parsed rest end-column)
  "parsed rest end-column"
  (list 'parse-remainder parsed rest end-column))

(defun bnf-mode-is-parse-remainder? (v)
  (bnf-mode-base-predicate v 4 'parse-remainder))

(defun bnf-mode-parse-remainder-1 (v)
  (nth 1 v))

(defun bnf-mode-parse-remainder-2 (v)
  (nth 2 v))

(defun bnf-mode-parse-remainder-3 (v)
  (nth 3 v))


;; Data structure for the syntax parenthese
(defun bnf-mode-make-syntax-paren (location type-left content type-right)
  "location type-left content type-right"
  (list 'syntax-paren location type-left content type-right))

(defun bnf-mode-is-syntax-paren? (v)
  (bnf-mode-base-predicate v 5 'syntax-paren))

(defun bnf-mode-syntax-paren-1 (v)
  (bnf-mode-first v))

(defun bnf-mode-syntax-paren-2 (v)
  (bnf-mode-second v))

(defun bnf-mode-syntax-paren-3 (v)
  (bnf-mode-third v))

;;;


;; For now the syntax constant is based on the constant used for the
;; bnf, should be changed to have its own datatype at some point.
(defun bnf-mode-make-syntax-constant (location constant)
  "Same as make-constant: location constant"
  (bnf-mode-make-constant location constant))

(defun bnf-mode-is-syntax-constant? (v)
  (bnf-mode-is-constant? v))

(defun bnf-mode-syntax-constant-1 (v)
  (bnf-mode-first v))

;;;

(defun bnf-mode-make-syntax-quote (location content)
  (list 'syntax-quote location content))

(defun bnf-mode-is-syntax-quote? (v)
  (bnf-mode-base-predicate v 3 'syntax-quote))

(defun bnf-mode-syntax-quote-1 (v)
  (bnf-mode-first v))


;;;

(defun bnf-mode-make-syntax-comma (location content)
  (list 'syntax-comma location content))

(defun bnf-mode-is-syntax-comma? (v)
  (bnf-mode-base-predicate v 3 'syntax-comma))

(defun bnf-mode-syntax-comma-1 (v)
  (bnf-mode-first v))


;;;

(defun bnf-mode-make-syntax-backward-quote (location content)
  (list 'syntax-backward-quote location content))

(defun bnf-mode-is-syntax-backward-quote? (v)
  (bnf-mode-base-predicate v 3 'syntax-backward-quote))

(defun bnf-mode-syntax-backward-quote-1 (v)
  (bnf-mode-first v))


;;;;;;;;;;;;;; BEGIN Look up reference data types ;;;;;;;;;;;;

(defun bnf-mode-make-reference-buildin (buildin)
  (list 'reference-buildin buildin))

(defun bnf-mode-is-reference-buildin? (v)
  (bnf-mode-base-predicate v 2 'reference-buildin))

(defun bnf-mode-reference-buildin-1 (v)
  (nth 1 v))


;;;

(defun bnf-mode-make-reference-not-found (v)
  (list 'reference-not-found v))

(defun bnf-mode-is-reference-not-found? (v)
  (bnf-mode-base-predicate v 2 'reference-not-found))

(defun bnf-mode-reference-not-found-1 (v)
  (nth 1 v))

;;; 

(defun bnf-mode-make-reference-found (v)
  (list 'reference-found v))

(defun bnf-mode-is-reference-found? (v)
  (bnf-mode-base-predicate v 2 'reference-found))

(defun bnf-mode-reference-found-1 (v)
  (nth 1 v))

;;;

;; Used to indicate that a certain remainder is from the result of
;; evaluating a group-ref.
(defun bnf-mode-make-remainder-group-ref (v)
  (list 'remainder-group-ref v))

(defun bnf-mode-is-remainder-group-ref? (v)
  (bnf-mode-base-predicate v 2 'remainder-group-ref))

(defun bnf-mode-remainder-group-ref-1 (v)
  (nth 1 v))

;;;

;; Returns a remainder or res, iff res is an error.
;; arity-match, indicates if the arity of the matched was ok
;; constant-match, indicates if the constant(s) of the matched was ok.
(defun bnf-mode-make-remainder (res v arity-match constant-match paren-match)
  (if (and (bnf-mode-boolean? res) 
	   (not (bnf-mode-is-remainder? v)))
      (list 'remainder res v arity-match constant-match paren-match)
    (error "make-remainder, bnf-mode-first arg: result; bnf-mode-second arg: remainder, user input, got: %s %s" res v)))

(defun bnf-mode-is-remainder? (v)
  (bnf-mode-base-predicate v 6 'remainder))

(defun bnf-mode-remainder-1 (v)
  (nth 1 v))

(defun bnf-mode-remainder-2 (v)
  (nth 2 v))

(defun bnf-mode-remainder-3 (v)
  (nth 3 v))

(defun bnf-mode-remainder-4 (v)
  (nth 4 v))

(defun bnf-mode-remainder-5 (v)
  (nth 5 v))


;;;;;;;;; Errors

(defun bnf-mode-value-from-error (e)
  (nth 2 e))

;;;

(defun bnf-mode-is-error? (v)
  (or (bnf-mode-is-error-arity? v)
      (bnf-mode-is-error-constant? v)
      (bnf-mode-is-error-arity-nested? v)
      (bnf-mode-is-error-group-ref-expected-buildin? v)
      (bnf-mode-is-error-group-ref-expected-reference-nested? v)
      (bnf-mode-is-error-missing-paren? v)
      (bnf-mode-is-error-no-match-in-reference? v)
      (bnf-mode-is-error-unmatched-input? v)
      (bnf-mode-is-build-in-error? v)
      (bnf-mode-is-error-group-ref-not-found? v)))
;;;

(defun bnf-mode-make-error-arity-with-multiplier (v bnf-location min-length actual-length)
  (list 'error-arity-with-multiplier bnf-location v min-length actual-length))


;;;

(defun bnf-mode-make-error-arity (v bnf-location expected-length actual-length)
  (if (not (bnf-mode-is-bnf-location? bnf-location))
      (error "second must be bnf-location: %s" bnf-location)
    (list 'error-arity bnf-location v expected-length actual-length)))

(defun bnf-mode-is-error-arity? (v)
  (bnf-mode-base-predicate v 5 'error-arity))

(defun bnf-mode-error-arity-1 (v)
  (nth 3 v))

(defun bnf-mode-error-arity-2 (v)
  (nth 4 v))


;;;

(defun bnf-mode-make-error-arity-nested (v bnf-location arity-error other-error)
  (list 'error-arity-nested bnf-location v arity-error other-error))

(defun bnf-mode-is-error-arity-nested? (v)
  (bnf-mode-base-predicate v 5 'error-arity-nested))

;;;

(defun bnf-mode-make-error-constant (v bnf-location expected)
  (list 'error-constant bnf-location v expected))

(defun bnf-mode-is-error-constant? (v)
  (bnf-mode-base-predicate v 4 'error-constant))

(defun bnf-mode-error-constant-1 (v)
  (nth 3 v))

;;;

(defun bnf-mode-make-error-missing-paren (v bnf-location)
  (list 'error-missing-paren bnf-location v))

(defun bnf-mode-is-error-missing-paren? (v)
  (bnf-mode-base-predicate v 3 'error-missing-paren))

;;;

(defun bnf-mode-make-error-wrong-paren-type (v bnf-location)
  (list 'error-wrong-paren-type bnf-location v))

(defun bnf-mode-make-error-paren-type-mismatch-left (v bnf-location)
  (list 'error-paren-type-mismatch-left bnf-location v))

(defun bnf-mode-is-error-paren-type-mismatch-left? (v)
  (bnf-mode-base-predicate v 3 'error-paren-type-mismatch-left))

;;

(defun bnf-mode-make-error-paren-type-mismatch-right (v bnf-location)
  (list 'error-paren-type-mismatch-right bnf-location v))

(defun bnf-mode-is-error-paren-type-mismatch-right? (v)
  (bnf-mode-base-predicate v 3 'error-paren-type-mismatch-right))

;;

(defun bnf-mode-is-error-paren-type-mismatch? (v)
  (or (bnf-mode-is-error-paren-type-mismatch-left? v)
      (bnf-mode-is-error-paren-type-mismatch-right? v)))



;;;

(defun bnf-mode-make-error-unmatched-input (v bnf-location)
  (if (not (bnf-mode-is-bnf-location? bnf-location))
      (error "second must be bnf-location, got: %s" bnf-location)
    (list 'error-unmatched-input bnf-location v)))

(defun bnf-mode-is-error-unmatched-input? (v)
  (bnf-mode-base-predicate v 3 'error-unmatched-input))

;;;

(defun bnf-mode-make-error-group-ref-not-found (v bnf-location group-ref)
  (list 'error-group-ref-not-found bnf-location v group-ref))

(defun bnf-mode-is-error-group-ref-not-found? (v)
  (bnf-mode-base-predicate v 4 'error-group-ref-not-found))

(defun bnf-mode-error-group-ref-not-found-1 (v)
  (nth 3 v))

;;;

(defun bnf-mode-make-error-group-ref-expected-buildin (v bnf-location buildin-error)
  (list 'error-group-ref-expected-buildin bnf-location v buildin-error))

(defun bnf-mode-is-error-group-ref-expected-buildin? (v)
  (bnf-mode-base-predicate v 4 'error-group-ref-expected-buildin))

(defun bnf-mode-error-group-ref-expected-buildin-1 (e)
  (nth 3 e))


;;;

(defun bnf-mode-make-error-group-ref-expected-reference-nested (v bnf-location reference-error)
  (list 'error-group-ref-expected-reference-nested bnf-location v reference-error))

(defun bnf-mode-is-error-group-ref-expected-reference-nested? (v)
  (bnf-mode-base-predicate v 4 'error-group-ref-expected-reference-nested))

(defun bnf-mode-error-group-ref-expected-reference-nested-1 (v)
  (bnf-mode-first v))

(defun bnf-mode-error-group-ref-expected-reference-nested-2 (v)
  (bnf-mode-second v))

;;;

(defun bnf-mode-make-error-group-ref-expected-reference (v bnf-location)
  (list 'error-group-ref-expected-reference bnf-location v))


;;;

(defun bnf-mode-make-error-no-match-in-reference (v bnf-location ref)
  (list 'error-no-match-in-reference bnf-location v ref))

(defun bnf-mode-is-error-no-match-in-reference? (v)
  (bnf-mode-base-predicate v 4 'error-no-match-in-reference))

(defun bnf-mode-error-no-match-in-reference-1 (v)
  (nth 3 v))

;;;

(defun bnf-mode-make-error-plus-at-least-one-match (v bnf-location)
  (list 'error-plus-at-least-one-match bnf-location v))

(defun bnf-mode-is-error-plus-at-least-one-match? (v)
  (bnf-mode-base-predicate v 3 'error-plus-at-least-one-match))

;;;

(defun bnf-mode-make-error-missing-syntax (v bnf-location)
  (list 'error-missing-syntax bnf-location v))

(defun bnf-mode-is-error-missing-syntax? (v)
  (bnf-mode-base-predicate v 3 'error-missing-syntax))


;; This is an error made to never be selected.  It serves as a base
;; case in the fold functions where the better of two errors are
;; selected.
(defun bnf-mode-make-worst-possible-error ()
  (list 'worst-possible-error))

(defun bnf-mode-is-worst-possible-error? (v)
  (bnf-mode-base-predicate v 1 'worst-possible-error))


;;;

(defun bnf-mode-make-missing-quote (v bnf-loc)
  (list 'missing-quote bnf-loc v))

(defun bnf-mode-is-missing-quote? (v)
  (bnf-mode-base-predicate v 3 'missing-quote))

;;;



(provide 'data-structure)
;; A sexp parser to carry information about location of the syntax.

(require 'string-functions)
(require 'string-predicates)

(defun bnf-mode-strip-prefix-whitespace (v)
  (while (bnf-mode-is-str-white-space? (car v))
    (setq v (cdr v)))
  v)


(defun bnf-mode-strip-prefix-whitespace-count (v)
  (let ((c 0))
    (while (bnf-mode-is-str-white-space? (car v))
      (setq c (+ c 1))
      (setq v (cdr v)))
  c))

;(bnf-mode-strip-prefix-whitespace-count '(" " " " "\n" "a"))

(defun bnf-mode-combine-acc-and-value (acc v)
  (cond
   
   ((or (bnf-mode-is-syntax-paren? acc)
	(bnf-mode-is-constant? acc)
	(bnf-mode-is-syntax-quote? acc)
	(bnf-mode-is-syntax-backward-quote? acc)
	(bnf-mode-is-syntax-comma? acc))
    (append (list acc) (list v)))
   
   ((consp acc)
    (append acc (list v)))

   (t
    (list v))))



(defun bnf-mode-sexp-handle-constant (acc constant column-number)
  (if (> (length constant) 0)
      (bnf-mode-combine-acc-and-value 
       acc 
       (bnf-mode-make-constant 
	(bnf-mode-make-syntax-location 
	 (- column-number (length constant)) 
	 column-number)
	(intern (bnf-mode-trim-string constant))
	))
    acc))


(defun bnf-mode-make-comment-remainder (v end-column-offset)
  "v end-column-offset"
  (list 'comment-remainder v end-column-offset))

(defun bnf-mode-comment-remainder-1 (v)
  (nth 1 v))
(defun bnf-mode-comment-remainder-2 (v)
  (nth 2 v))

(defun bnf-mode-sexp-handle-comment (v)
  (let ((count 0))
    (while (and (car v) (not (string= "\n" (car v))))
      (setq v (cdr v))
      (setq count (+ 1 count)))
    (if (string= "\n" (car v))
	(progn
	  (setq v (cdr v))
	  (setq count (+ 1 count))))
    (bnf-mode-make-comment-remainder 
     v
     count)))


(defun bnf-mode-make-constant-remainder (v column-offset constant)
  (list 'constant-remainder v column-offset constant))

(defun bnf-mode-is-constant-remainder? (v)
  (bnf-mode-base-predicate v 3 'constant-remainder))

(defun bnf-mode-constant-remainder-1 (v)
  (nth 1 v))

(defun bnf-mode-constant-remainder-2 (v)
  (nth 2 v))

(defun bnf-mode-constant-remainder-3 (v)
  (nth 3 v))



(defun bnf-mode-retrieve-constant (v)
  (let ((constant ""))
    (while (bnf-mode-is-str-constant? (car v))
      (setq constant (concat constant (car v)))
      (setq v (cdr v)))
    (bnf-mode-make-constant-remainder v (length constant) constant)))


;;;

(defun bnf-mode-make-quote-context-remainder (v acc colmun-number)
  "v acc column-number"
  (list 'quote-context-remainder v acc colmun-number))

(defun bnf-mode-is-quote-context-remainder? (v)
  (bnf-mode-base-predicate v 4 'quote-context-remainder))
(defun bnf-mode-quote-context-remainder-1 (v)
  (nth 1 v))

(defun bnf-mode-quote-context-remainder-2 (v)
  (nth 2 v))

(defun bnf-mode-quote-context-remainder-3 (v)
  (nth 3 v))

;;;	    

(defun bnf-mode-handle-quote-context (v column-number f)
  (cond

   ((null v)
    (bnf-mode-make-quote-context-remainder
     nil
     (funcall f
;     (bnf-mode-make-syntax-quote
      (bnf-mode-make-syntax-location
       column-number
       (+ 1 column-number))
      nil)
     (+ 1 column-number)))
     

   ((or (bnf-mode-is-str-quote? (car v)) 
	(bnf-mode-is-str-comma? (car v)))
    (let ((res (bnf-mode-handle-quote-context (cdr v) (+ 1 column-number) f)))
      (bnf-mode-make-quote-context-remainder
       (bnf-mode-quote-context-remainder-1 res)
       ;(bnf-mode-make-syntax-quote
       (funcall 
	f
	(bnf-mode-make-syntax-location
	 column-number
	 (bnf-mode-syntax-location-2 
	  (bnf-mode-syntax-location-from-value (bnf-mode-quote-context-remainder-2 res))))
	(bnf-mode-quote-context-remainder-2 res))
       (bnf-mode-syntax-location-2 
	  (bnf-mode-syntax-location-from-value (bnf-mode-quote-context-remainder-2 res))))))
	
	

   ((bnf-mode-is-str-constant? (car v))
    (let ((right-const (bnf-mode-retrieve-constant v)))
      (bnf-mode-make-quote-context-remainder
       (bnf-mode-constant-remainder-1 right-const)
       
       ;(bnf-mode-make-syntax-quote
       (funcall 
	f
	(bnf-mode-make-syntax-location 
	 column-number
	 (+ 1 column-number (bnf-mode-constant-remainder-2 right-const)))
	(bnf-mode-make-constant 
	 (bnf-mode-make-syntax-location
	  (+ 1 column-number) 
	  (+ (+ 1 column-number) (length (bnf-mode-constant-remainder-3 right-const))))
	 (intern (bnf-mode-trim-string (bnf-mode-constant-remainder-3 right-const)))))
       (+ 1 column-number (bnf-mode-constant-remainder-2 right-const)))))

   ((bnf-mode-is-str-paren-begin? (car v))
    (let* ((result (bnf-mode-sexp-parse (cdr v) (+ 2 column-number) (+ 1 column-number) 0))
	   (paren (bnf-mode-make-syntax-paren
		   (bnf-mode-syntax-location-from-helper-1 (bnf-mode-parse-remainder-1 result))
		   (bnf-mode-derive-paren-type (car v))
		   (bnf-mode-paren-2 (bnf-mode-parse-remainder-1 result))
		   (bnf-mode-paren-1 (bnf-mode-parse-remainder-1 result))
		   )))
      (bnf-mode-make-quote-context-remainder
       (bnf-mode-parse-remainder-2 result)
       ;(bnf-mode-make-syntax-quote 
       (funcall 
	f
	(bnf-mode-make-syntax-location
	 column-number
	 (bnf-mode-syntax-location-2 (bnf-mode-syntax-location-from-value paren)))
	paren)
       (bnf-mode-syntax-location-2 (bnf-mode-syntax-location-from-helper-1 (bnf-mode-parse-remainder-1 result))))))

   (t
    (bnf-mode-make-quote-context-remainder v nil column-number))))

;; (bnf-mode-handle-quote-context '(" " "b" ")") 0 'bnf-mode-make-syntax-comma)






(defun bnf-mode-sexp-parse-main (str)
  (bnf-mode-sexp-replace-quotes
   (bnf-mode-parse-remainder-1 
    (bnf-mode-sexp-parse (split-string str "" t) 0 0 0))))

(defun bnf-mode-sexp-parse (v  column-number_  paren-column-number-begin cnt)
  (let ((acc nil)
	(constant "")
	(r nil)
	(column-number column-number_))
    (while v
      (cond

       ((bnf-mode-is-str-comment? (car v))
	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	(setq constant "")
	(let ((comment-res (bnf-mode-sexp-handle-comment v)))
	  (setq v (bnf-mode-comment-remainder-1 comment-res))
	  (setq column-number (+ column-number (bnf-mode-comment-remainder-2 comment-res)))))

       ((bnf-mode-is-str-string? v)
	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	(setq constant "")
       	(let ((r (bnf-mode-is-str-string? v)))
	  (setq column-number (+ column-number (bnf-mode-parse-remainder-3 r)))
	  (setq acc (bnf-mode-sexp-handle-constant 
		     acc 
		     (bnf-mode-parse-remainder-1 r) 
		     column-number))
	  (setq v (bnf-mode-parse-remainder-2 r))))

       
       ((bnf-mode-is-str-quote? (car v))
       	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	(setq constant "")
       	(let ((quote-res (bnf-mode-handle-quote-context 
			  (bnf-mode-strip-prefix-whitespace (cdr v))
			  (+ (bnf-mode-strip-prefix-whitespace-count (cdr v)) column-number)
			  'bnf-mode-make-syntax-quote)))
       	  (setq v (bnf-mode-quote-context-remainder-1 quote-res))
       	  (setq acc (bnf-mode-combine-acc-and-value acc (bnf-mode-quote-context-remainder-2 quote-res)))
       	  (setq column-number (bnf-mode-quote-context-remainder-3 quote-res))))

       ((bnf-mode-is-str-comma? (car v))
       	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
       	(setq constant "")
       	(let ((quote-res (bnf-mode-handle-quote-context 
			  (bnf-mode-strip-prefix-whitespace (cdr v))
			  (+ (bnf-mode-strip-prefix-whitespace-count (cdr v)) column-number)
			  'bnf-mode-make-syntax-comma)))
       	  (setq v (bnf-mode-quote-context-remainder-1 quote-res))
       	  (setq acc (bnf-mode-combine-acc-and-value acc (bnf-mode-quote-context-remainder-2 quote-res)))
       	  (setq column-number (bnf-mode-quote-context-remainder-3 quote-res))))



       ((bnf-mode-is-str-backward-quote? (car v))
       	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
       	(setq constant "")
       	(let ((quote-res (bnf-mode-handle-quote-context 
			  (bnf-mode-strip-prefix-whitespace (cdr v))
			  (+ (bnf-mode-strip-prefix-whitespace-count (cdr v)) column-number)
			  'bnf-mode-make-syntax-backward-quote)))
       	  (setq v (bnf-mode-quote-context-remainder-1 quote-res))
       	  (setq acc (bnf-mode-combine-acc-and-value acc (bnf-mode-quote-context-remainder-2 quote-res)))
       	  (setq column-number (bnf-mode-quote-context-remainder-3 quote-res))))


       ((bnf-mode-is-str-new-line? (car v))
	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	(setq constant "")
	(setq column-number (+ 1 column-number))
	(setq v (cdr v)))

       ((bnf-mode-is-str-constant? (car v))
	(if (null (cdr v))
	    (progn
	      (setq acc (bnf-mode-sexp-handle-constant 
			 acc 
			 (concat constant (car v))
			 (+ 1 column-number)))
	      (setq constant ""))
	  (setq constant (concat constant (car v))))
	(setq column-number (+ 1 column-number))
	(setq v (cdr v)))
       
       ((bnf-mode-is-str-white-space? (car v))
	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	(setq constant "")
	(setq column-number (+ 1 column-number))
	(setq v (cdr v)))
       
       ((bnf-mode-is-str-paren-begin? (car v))
	(setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	(setq constant "")
	(let* ((result (bnf-mode-sexp-parse (cdr v) (+ 1 column-number) column-number (+ 1 cnt)))
	       (paren (bnf-mode-make-syntax-paren
		       (bnf-mode-syntax-location-from-helper-1 (bnf-mode-parse-remainder-1 result))
		       (bnf-mode-derive-paren-type (car v))
		       (bnf-mode-paren-2 (bnf-mode-parse-remainder-1 result))
		       (bnf-mode-paren-1 (bnf-mode-parse-remainder-1 result))
		       )))
	  (setq acc (bnf-mode-combine-acc-and-value acc paren))
	  (setq v (bnf-mode-parse-remainder-2 result))
	  (setq column-number 
		(bnf-mode-syntax-location-2 
		 (bnf-mode-syntax-location-from-helper-1 (bnf-mode-parse-remainder-1 result))))))


       ((bnf-mode-is-str-paren-end? (car v))
	(if (and (> (length constant) 0) (not (bnf-mode-is-str-white-space? constant)))
	    (progn
	      (setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	      (setq constant "")))
	(setq r (bnf-mode-make-parse-remainder 
		 (bnf-mode-make-syntax-paren-helper
		  (bnf-mode-make-syntax-location 
		   paren-column-number-begin 
		   (+ 1 column-number))
		  (bnf-mode-derive-paren-type (car v))
		  acc)
		 (cdr v)
		 (+ 1 column-number)))
	(setq v nil))

       ))

    (if (null r)
	(progn
	  (setq acc (bnf-mode-sexp-handle-constant acc constant column-number))
	  (bnf-mode-make-parse-remainder acc v -1))
      r)))


(defun bnf-mode-make-syntax-paren-with-constant (syntax-location const content )
  (bnf-mode-make-syntax-paren
   syntax-location
   (bnf-mode-make-paren-type-c)
   (list
    (bnf-mode-make-syntax-constant
     syntax-location
     const)
    content)
   (bnf-mode-make-paren-type-c)))

;; '<quotation> is replaced to (quote <quotation>)
(defun bnf-mode-sexp-replace-quotes (v)
  (cond

   ((bnf-mode-is-syntax-paren? v)
    (bnf-mode-make-syntax-paren
     (bnf-mode-syntax-location-from-value v)
     (bnf-mode-syntax-paren-1 v)
     (bnf-mode-sexp-replace-quotes (bnf-mode-syntax-paren-2 v))
     (bnf-mode-syntax-paren-3 v)))

   ((bnf-mode-is-syntax-comma? v) ;; ,<x> --> (unquote <x>)
    (bnf-mode-make-syntax-paren-with-constant
     (bnf-mode-syntax-location-from-value v)
     'unquote
     (bnf-mode-syntax-comma-1 v)))
   
   ((bnf-mode-is-syntax-quote? v)
    (bnf-mode-make-syntax-paren-with-constant
     (bnf-mode-syntax-location-from-value v)
     'quote
     (bnf-mode-syntax-quote-1 v)))

   ((bnf-mode-is-syntax-backward-quote? v)
    (bnf-mode-make-syntax-paren-with-constant
     (bnf-mode-syntax-location-from-value v)
     'quasiquote
     (bnf-mode-syntax-backward-quote-1 v)))

   ((bnf-mode-is-syntax-constant? v)
    v)

   ((bnf-mode-is-value-list? v)
    (bnf-mode-fold-left
     v
     (lambda (bnf-mode-x rest)
       (append 
	rest
	(list (bnf-mode-sexp-replace-quotes bnf-mode-x))))
     (lambda () '())))))


;; A temp syntax paren
(defun bnf-mode-make-syntax-paren-helper (loc type content)
  (list 'syntax-paren-helper loc type content))

(defun bnf-mode-syntax-location-from-helper-1 (v)
  (nth 1 v))

;;;



;; Helpers
(defun bnf-mode-length-of-sexp-value (v)
  (cond 
   ((null v)
    0)

   ((bnf-mode-is-constant? v)
    1)

   ((bnf-mode-is-syntax-quote? v)
    1)

   ((bnf-mode-is-syntax-comma? v)
    1)

   ((bnf-mode-is-syntax-backward-quote? v)
    1)

   ((bnf-mode-is-syntax-paren? v)
    1)

   (t
    (length v))))


(defun bnf-mode-nested-length-of-sexp-value (v)
  (cond 
   ((null v)
    0)

   ((bnf-mode-is-constant? v)
    1)

   ((bnf-mode-is-syntax-quote? v)
    (+ 1 (bnf-mode-nested-length-of-sexp-value (bnf-mode-syntax-quote-1 v))))

   ((bnf-mode-is-syntax-comma? v)
    (+ 1 (bnf-mode-nested-length-of-sexp-value (bnf-mode-syntax-comma-1 v))))

   ((bnf-mode-is-syntax-backward-quote? v)
    (+ 1 (bnf-mode-nested-length-of-sexp-value (bnf-mode-syntax-backward-quote-1 v))))

   ((bnf-mode-is-syntax-paren? v)
    (+ 1 (bnf-mode-nested-length-of-sexp-value (bnf-mode-syntax-paren-2 v))))

   ((bnf-mode-is-value-list? v)
    (bnf-mode-fold-left
     v
     (lambda (bnf-mode-x rest)
       (+ (bnf-mode-nested-length-of-sexp-value bnf-mode-x) rest))
     (lambda () 0)))))



(defun bnf-mode-ui-car (v)
  (cond
   ((bnf-mode-is-constant? v)
    v)

   ((bnf-mode-is-syntax-quote? v)
    v)

   ((bnf-mode-is-syntax-comma? v)
    v)

   ((bnf-mode-is-syntax-backward-quote? v)
    v)

   ((bnf-mode-is-syntax-paren? v)
    v)    

   ((bnf-mode-is-value-list? v)
    (car v))

   (t
    v)))


;; (c (a) c)  --> nil
;; (c (a b) c) --> (b)
(defun bnf-mode-ui-cdr (v)
  (cond

   ((bnf-mode-is-syntax-paren? v)
    nil)

   ((bnf-mode-is-constant? v)
    nil)

   ((bnf-mode-is-syntax-quote? v)
    nil)

   ((bnf-mode-is-syntax-backward-quote? v)
    nil)

   ((bnf-mode-is-syntax-comma? v)
    nil)

   ((bnf-mode-is-value-list? v)
    (if (null (cdr (cdr v)))
	(car (cdr v))
      (cdr v)))

   (t 
    nil)))


(provide 'sexp-parser)
;; <a> ::= (abc {<b>}*) | (abc <c>)
;; <b> ::= (<d> <d>)
;; <c> ::= (<d> <d>)
;; <d> ::= a | b | c


(require 'string-predicates)


;; A data structure required by the parser.
(defun bnf-mode-make-bnf-parse-remainder (v remainder column-start column-end)
  "parsed remainder column-start column-end"
  (list 'bnf-parse-remainder v remainder column-start column-end))

(defun bnf-mode-bnf-parse-remainder-1 (v)
  (nth 1 v))

(defun bnf-mode-bnf-parse-remainder-2 (v)
  (nth 2 v))

(defun bnf-mode-bnf-parse-remainder-3 (v)
  (nth 3 v))

(defun bnf-mode-bnf-parse-remainder-4 (v)
  (nth 4 v))




;; If the bnf-mode-first of the list is optional white space
;; followed by a group-name, such as: <abc>, return the 
;; group-name, the remainder and the column-offset
(defun bnf-mode-str-group-name (v)
  (let ((r nil) (cn 0) (group-name "") (group-begin-cn -1))
    (while v
      (cond

       ((or (bnf-mode-is-str-white-space? (car v)) (bnf-mode-is-str-new-line? (car v)))
	(if (and (> group-begin-cn -1) (> (length group-name) 0))
	    (progn
	      (setq v nil)
	      (setq r nil))
	  (progn
	    (setq v (cdr v))
	    (setq cn (+ 1 cn)))))

       ((bnf-mode-is-str-arrow-begin? (car v))
	(if (> (length group-name) 0)
	    (progn
	      (setq r nil)
	      (setq v nil))
	  (progn
	    (setq group-begin-cn cn)
	    (setq v (cdr v))
	    (setq cn (+ 1 cn)))))

       ((bnf-mode-is-str-bnf-constant? (car v))
	(setq group-name (concat group-name (car v)))
	(setq v (cdr v))
	(setq cn (+ 1 cn)))

       ((bnf-mode-is-str-arrow-end? (car v))
	(setq r (bnf-mode-make-bnf-parse-remainder (concat "<" group-name ">") (cdr v) group-begin-cn (+ 1 cn)))
	(setq v nil))

       (t
	(setq v nil))

       ))
    (if r
	r
      nil)))

(defun bnf-mode-str-define (v)
  (let ((r nil) (cn 0) (vv v))
    (while vv
      (cond
     
       ((or (bnf-mode-is-str-white-space? (car vv)) (bnf-mode-is-str-new-line? (car vv)))
	(setq vv (cdr vv))
	(setq cn (+ 1 cn)))
     
       (t
	(setq v vv)
	(setq vv nil))
     
       ))
    (if (and
	 (string= (nth 0 v) ":")
	 (string= (nth 1 v) ":")
	 (string= (nth 2 v) "="))
	(bnf-mode-make-bnf-parse-remainder "::=" (nthcdr 3 v) cn (+ cn 3))
      nil)))


;; (bnf-mode-str-group-name '(" " "\n" " " "<" "a" ">" ""))

;; (bnf-mode-str-define
;;  (bnf-mode-bnf-parse-remainder-2 
;;   (bnf-mode-str-group-name '(" " " " " " "<" "a" "b" ">" " " " " "\n" " " " " ":" ":" "="))))

;; (bnf-mode-str-define '(" " " " " " " " ":" ":" "="))

(defun bnf-mode-is-str-define? (v cn)
  (let ((group (bnf-mode-str-group-name v)))
    (if group
	(let ((define (bnf-mode-str-define (bnf-mode-bnf-parse-remainder-2 group))))
	  (if define
	      (bnf-mode-make-bnf-parse-remainder
	       (bnf-mode-bnf-parse-remainder-1 group)
	       (bnf-mode-bnf-parse-remainder-2 define)
	       (+ cn (bnf-mode-bnf-parse-remainder-3 group))
	       (+ cn 
		  (bnf-mode-bnf-parse-remainder-4 group)
		  (bnf-mode-bnf-parse-remainder-4 define)))
	    nil))
      nil)))



(defun bnf-mode-is-value? (v)
  (or
   (bnf-mode-is-quote? v)
   (bnf-mode-is-backward-quote? v)
   (bnf-mode-is-comma? v)
   (bnf-mode-is-group-ref? v)
   (bnf-mode-is-star? v)
   (bnf-mode-is-plus? v)
   (bnf-mode-is-constant? v)
   (bnf-mode-is-paren? v)))



(defun bnf-mode-is-bnf-list? (v)
  "Checks if the givne list is a list of bnf-constructors."
  (and (not (null v))
       (not (bnf-mode-is-constructor? v))
       (bnf-mode-is-constructor? (car v))))
;; Assumption: If 1 is a constructor, then so is 2 ... n.
  


(defun bnf-mode-last-element-of-list (v)
  (if (not (consp v))
      nil
    (car (nthcdr (- (length v) 1) v))))

(defun bnf-mode-remove-last-element-of-list (v)
  (bnf-mode-n-first-of-list (- (length v) 1)  v))

;; (bnf-mode-remove-last-element-of-list '(1 2 3 4))


(defun bnf-mode-extend-parsed-star-plus (acc f-cons)
  (cond


   ((bnf-mode-is-constant? acc) 
    (bnf-mode-make-constant (bnf-mode-extend-bnf-location-end
		    (bnf-mode-bnf-location-from-value acc)
		    1)
		   (intern
		    (concat 
		     (symbol-name (bnf-mode-constant-1 acc))
		     (if (equal f-cons 'bnf-mode-make-star)
			 "*"
		       "+")))))

   ((or (bnf-mode-is-group-ref? acc)
	(bnf-mode-is-paren? acc))
    (funcall f-cons
	     (bnf-mode-extend-bnf-location-end
	      (bnf-mode-bnf-location-from-value acc)
	      1)
	     (list acc)))


   ((bnf-mode-is-member? acc)
    ;; The star should be associated with the last element in the list.
    (let ((last-elm (bnf-mode-last-element-of-list (bnf-mode-member-1 acc))))
      (bnf-mode-make-member
       (bnf-mode-extend-bnf-location-end
	(bnf-mode-bnf-location-from-value acc) 1)
       (append
	(bnf-mode-remove-last-element-of-list (bnf-mode-member-1 acc))
	(list
	 (funcall f-cons
		 (bnf-mode-extend-bnf-location-end
		  (bnf-mode-bnf-location-from-value last-elm) 1)
		 (list last-elm)))))))

   
   ((bnf-mode-is-value-list? acc)
    (let ((last (bnf-mode-last-element-of-list acc)))
      (append
       (bnf-mode-remove-last-element-of-list acc)
       (funcall f-cons
		(bnf-mode-extend-bnf-location-end
		 (bnf-mode-bnf-location-from-value last) 1)
		last))))
	

   (t
    (error "extend-parsed-star: unhandled case: %s " acc ))))
    

(defun bnf-mode-extend-bnf-location-end (bnf-loc offset)
  "Given the bnf-loc, adds the offset to the end of the bnf-loc"
  (bnf-mode-make-bnf-location
   (bnf-mode-bnf-location-1 bnf-loc)
   (+ (bnf-mode-bnf-location-2 bnf-loc) offset)))


(bnf-mode-bnf-location-from-value
 '(quote (bnf-location 8 10) (constant (bnf-location 9 10) a)))

(defun bnf-mode-extend-parsed (acc v)
  (cond
   
   ((null acc)
    (bnf-mode-make-member 
     (bnf-mode-bnf-location-from-value v)
     (list v)))

   ((and (bnf-mode-is-value? acc) 
	 (bnf-mode-is-value? v))
    (bnf-mode-make-member 
     (bnf-mode-extend-bnf-location
      (bnf-mode-bnf-location-from-value acc)
      (bnf-mode-bnf-location-from-value v))
     (append
      (list acc)
      (list v))))
    

   ((and (null acc) (bnf-mode-is-value? v))
    (bnf-mode-make-member 
     (bnf-mode-bnf-location-from-value v)
     (list v)))

   ;; (m (a)) + b --> (m (a b))
   ((and (bnf-mode-is-member? acc)
	 (bnf-mode-is-value? v))
    (bnf-mode-make-member
     (bnf-mode-extend-bnf-location 
      (bnf-mode-bnf-location-from-value acc)
      (bnf-mode-bnf-location-from-value v))
     (append 
      (bnf-mode-member-1 acc)
      (list v))))


   ((and (bnf-mode-is-value-list? acc)
	 (bnf-mode-is-value? v))
    (append
     acc
     (list v)))


   (t
    (error "extend-parsed, unable to combine: %s and %s" acc v))))



(defun bnf-mode-combine-two-groups (a b)
  (cond 

   ((and (null a) (bnf-mode-is-group? b))
    (list b))

   ((and (consp a) (bnf-mode-is-group? b))
    (append a (list b)))))
     

(defun bnf-mode-extend-parsed-with-constant (acc v)
  (cond

   ((null acc)
    v)

   ((and (bnf-mode-is-value-list? acc)
	 (bnf-mode-is-value? v))
    (append
     acc
     (list v)))

   ((and (bnf-mode-is-value? acc) 
	 (bnf-mode-is-value? v))
    (list acc v))

   ((bnf-mode-is-member? acc)
    (bnf-mode-extend-parsed acc v))

   (t
    (error "extend-parsed-with-constant unhandled case: %s %s" acc v))

))




(defun bnf-mode-combine-two-members (a b)
  (cond

   ((and (null a)
	 (bnf-mode-is-value? b))
    (bnf-mode-make-members
     (bnf-mode-bnf-location-from-value b)
     (list (bnf-mode-make-member
	    (bnf-mode-bnf-location-from-value b)
	    (list b)))))

   ((and (null a)
	 (bnf-mode-is-value-list? b))
    (let ((bnf-loc (bnf-mode-extend-bnf-location
		    (bnf-mode-bnf-location-from-value (car b))
		    (bnf-mode-bnf-location-from-value (bnf-mode-last-element-of-list b)))))
    (bnf-mode-make-members
     bnf-loc
     (list (bnf-mode-make-member
	    bnf-loc
	    b)))))


   ((and (null a)
	 (bnf-mode-is-member? b))
    (bnf-mode-make-members
     (bnf-mode-bnf-location-from-value b)
     (list b)))

   ((and (bnf-mode-is-member? a)
	 (null b))
    (bnf-mode-make-members
     (bnf-mode-bnf-location-from-value a)
     (list a)))

   ((and (bnf-mode-is-member? a)
	 (bnf-mode-is-member? b))
    (bnf-mode-make-members
     (bnf-mode-extend-bnf-location
      (bnf-mode-bnf-location-from-value a)
      (bnf-mode-bnf-location-from-value b))
      (list a b)))

   ((and (bnf-mode-is-members? a)
	 (bnf-mode-is-member? b))
    (bnf-mode-make-members
     (bnf-mode-extend-bnf-location
      (bnf-mode-bnf-location-from-value a)
      (bnf-mode-bnf-location-from-value b))
     (append (bnf-mode-members-1 a)
	     (list b))))

   ((and (bnf-mode-is-members? a)
	 (bnf-mode-is-value? b))
    (bnf-mode-make-members
     (bnf-mode-extend-bnf-location
      (bnf-mode-bnf-location-from-value a)
      (bnf-mode-bnf-location-from-value b))
     (append (bnf-mode-members-1 a)
	     (list 
	      (bnf-mode-make-member 
	       (bnf-mode-bnf-location-from-value b)
	       (list b))))))


   ((and (bnf-mode-is-members? a)
	 (bnf-mode-is-value-list? b))
    (bnf-mode-make-members
     (bnf-mode-extend-bnf-location
      (bnf-mode-bnf-location-from-value a)
      (bnf-mode-bnf-location-from-value b))
     (append (bnf-mode-members-1 a)
	     (list 
	      (bnf-mode-make-member 
	       (bnf-mode-bnf-location-from-value b)
	       b)))))



   (t
    (error "combine-two-members: %s %s" a b))
))




(defun bnf-mode-bnf-parse-group-name-context (v column-number)
  (let ((constant nil) (r nil))
    (while v
      (cond
       ((bnf-mode-is-str-bnf-constant? (car v))
	(progn
	  (setq constant (concat constant (car v)))
	  (setq v (cdr v))))

       ((bnf-mode-is-str-arrow-end? (car v))
	(progn
	  (setq r (bnf-mode-make-parse-remainder constant (cdr v) (+ column-number (length constant))))
	  (setq v nil)))

       (t
	(error "Wrong BNF-syntax: %s" v))))

    (if (null r)
	(error "Missing end arrow \">\"")
      r)))


(defun bnf-mode-is-group-name-ref? (v column-number)
  (let ((constant nil) (r nil) (start-arr nil))
    (while v
      (cond

       ((bnf-mode-is-str-arrow-begin? (car v))
	(setq v (cdr v))
	(setq start-arr t))

       ((bnf-mode-is-str-bnf-constant? (car v))
	(if (not start-arr)
	    (progn
	      (setq r nil)
	      (setq v nil))
	  (progn
	    (setq constant (concat constant (car v)))
	    (setq v (cdr v)))))

       ((bnf-mode-is-str-arrow-end? (car v))
	(if (and start-arr (< 0 (length constant)))
	  (progn
	    (setq r (bnf-mode-make-parse-remainder constant (cdr v) (+ column-number (length constant))))
	    (setq v nil))
	  (progn
	    (setq v nil)
	    (setq r nil))))


       (t
	(setq v nil)
	nil)))
    r))
    


;; (bnf-mode-bnf-parse-group-name-context '("a" "b" "c" ">" "s") 0)

;; The content after "::=" is the bnf-mode-first member, after that they are
;; seperated by | and at the end a new production or EOF.





(defun bnf-mode-is-member-context-end? (v member-context)
  (and member-context 
;       (or (bnf-mode-is-str-define? v)
	   (null v)))
  

(defun bnf-mode-bnf-parse-main (str)
  (let* ((result (bnf-mode-bnf-parse (split-string str "" t) 0 nil nil))
	 (outer-loc (bnf-mode-make-bnf-location
		     0
		     (bnf-mode-bnf-parse-remainder-4 result))))
    (if (not (null (bnf-mode-bnf-parse-remainder-2 result)))
	(error "Bnf not fully parsed: %s" result)
      (bnf-mode-bnf-replace-desugar 
       (bnf-mode-make-bnf
	outer-loc
	(bnf-mode-make-groups
	 outer-loc
	 (bnf-mode-bnf-parse-remainder-1 result)))))))


;; builds a round parenthesis with the constant and the input:
;; '(a b) ==> (paren c (a b))
(defun bnf-mode-make-paren-with-constant (bnf-location const content )
  (bnf-mode-make-paren
   bnf-location
   (bnf-mode-make-paren-type-c)
   (list
    (bnf-mode-make-constant
     bnf-location
     const)
    content)))


;; Replacements for Scheme syntactic sugar.
(defun bnf-mode-bnf-replace-desugar (v)
  (cond
   
   ((bnf-mode-is-constant? v)
    v)

   ((bnf-mode-is-quote? v)       ;; 'a ==> (quote a) ==> (paren c (constant quote) (constant a))
    (bnf-mode-make-paren-with-constant 
     (bnf-mode-bnf-location-from-value v)
     'quote 
     (bnf-mode-bnf-replace-desugar (bnf-mode-quote-1 v))))

   ((bnf-mode-is-comma? v)       ;; ,a ==> (unquote a) ==> (paren c (constant unqoute) (constant a))
    (bnf-mode-make-paren-with-constant 
     (bnf-mode-bnf-location-from-value v)
     'unquote 
     (bnf-mode-bnf-replace-desugar (bnf-mode-comma-1 v))))


   ((bnf-mode-is-backward-quote? v) ;; `a ==> (backward-quote a) ==> (paren c (constant backward-quote) (constant a))
    (bnf-mode-make-paren-with-constant 
     (bnf-mode-bnf-location-from-value v)
     'quasiquote
     (bnf-mode-bnf-replace-desugar (bnf-mode-backward-quote-1 v))))
  
   
   ((bnf-mode-is-paren? v)
    (bnf-mode-make-paren
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-paren-1 v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-paren-2 v))))

   ((bnf-mode-is-group-ref? v)
    v)

   ((bnf-mode-is-star? v)
    (bnf-mode-make-star
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-star-1 v))))

   ((bnf-mode-is-plus? v)
    (bnf-mode-make-plus
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-plus-1 v))))

   ((bnf-mode-is-member? v)
    (bnf-mode-make-member
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-member-1 v))))

   ((bnf-mode-is-members? v)
    (bnf-mode-make-members
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-members-1 v))))

   ((bnf-mode-is-group? v)
    (bnf-mode-make-group
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-group-1 v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-group-2 v))))

   ((bnf-mode-is-groups? v)
    (bnf-mode-make-groups
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-groups-1 v))))

   ((bnf-mode-is-bnf? v)
    (bnf-mode-make-bnf
     (bnf-mode-bnf-location-from-value v)
     (bnf-mode-bnf-replace-desugar (bnf-mode-bnf-1 v))))

   ((bnf-mode-is-bnf-list? v)
    (bnf-mode-fold-left
     v
     (lambda (bnf-mode-x rest)
       (append 
	rest
	(list (bnf-mode-bnf-replace-desugar bnf-mode-x))))
     (lambda () '())))

   ))
     





(defun bnf-mode-handle-bnf-quote-context (v column-number f)
  (cond

   ((bnf-mode-is-str-quote? (car v))
    (let ((res (bnf-mode-handle-bnf-quote-context (cdr v) (+ 1 column-number) f)))
      (bnf-mode-make-quote-context-remainder
       (bnf-mode-quote-context-remainder-1 res)
       (funcall f
       ;(bnf-mode-make-quote
	(bnf-mode-make-bnf-location
	 column-number
	 (bnf-mode-bnf-location-2 
	  (bnf-mode-bnf-location-from-value (bnf-mode-quote-context-remainder-2 res))))
	(bnf-mode-quote-context-remainder-2 res))
       (bnf-mode-bnf-location-2 
	  (bnf-mode-bnf-location-from-value (bnf-mode-quote-context-remainder-2 res))))))

   ((bnf-mode-is-str-arrow-begin? (car v))
    (let* ((group-name (bnf-mode-bnf-parse-group-name-context (cdr v) column-number))
	   (group-constant (concat "<" (bnf-mode-parse-remainder-1 group-name) ">")))
      (bnf-mode-make-quote-context-remainder
       (bnf-mode-parse-remainder-2 group-name)
       (funcall f
       ;(bnf-mode-make-quote
	(bnf-mode-make-bnf-location
	 column-number	 
	 (+ 3 (bnf-mode-parse-remainder-3 group-name)))
	(bnf-mode-make-group-ref
	 (bnf-mode-make-bnf-location
	  (+ 1 column-number)
	  (+ 3 (bnf-mode-parse-remainder-3 group-name)))
	 (intern group-constant)))
       (+ 3 (bnf-mode-parse-remainder-3 group-name)))))
			    


   ((bnf-mode-is-str-constant? (car v))
    (let ((right-const (bnf-mode-retrieve-constant v)))
      (bnf-mode-make-quote-context-remainder
       (bnf-mode-constant-remainder-1 right-const)
       (funcall f
       ;(bnf-mode-make-quote
	(bnf-mode-make-bnf-location
	 column-number
	 (+ 1 column-number (bnf-mode-constant-remainder-2 right-const)))
	(bnf-mode-make-constant 
	 (bnf-mode-make-bnf-location
	  (+ 1 column-number) 
	  (+ (+ 1 column-number) (length (bnf-mode-constant-remainder-3 right-const))))
	 (intern (bnf-mode-trim-string (bnf-mode-constant-remainder-3 right-const)))))
       (+ column-number (length (bnf-mode-constant-remainder-3 right-const))))))

   ((bnf-mode-is-str-paren-begin? (car v))
    (let* ((paren-type (bnf-mode-derive-paren-type (car v)))
	   (inner (bnf-mode-bnf-parse (cdr v)
			     (+ 2 column-number)
			     t
			     paren-type))
	   (inner-value (if (bnf-mode-is-member? (bnf-mode-bnf-parse-remainder-1 inner))
			    (bnf-mode-member-1 (bnf-mode-bnf-parse-remainder-1 inner))
			  (bnf-mode-bnf-parse-remainder-1 inner))))

      (bnf-mode-make-quote-context-remainder
       (bnf-mode-bnf-parse-remainder-2 inner)
       (funcall f
       ;(bnf-mode-make-quote 
	(bnf-mode-make-bnf-location
	 column-number
	 (bnf-mode-bnf-parse-remainder-4 inner))
	(bnf-mode-make-paren
	 (bnf-mode-make-bnf-location
	  (+ 1 column-number)
	  (bnf-mode-bnf-parse-remainder-4 inner))
	 (bnf-mode-derive-paren-type (car v))
	 inner-value))
	(bnf-mode-bnf-parse-remainder-4 inner))))

   (t
    (bnf-mode-make-quote-context-remainder v nil column-number))))

;;;

(defun bnf-mode-multiplier-type-1 (v)
  (nth 3 v))

;;

(defun bnf-mode-make-independent-multiplier (v-rest column-start type)
  "v-rest star-column-start type"
  (list 'independent-multiplier v-rest column-start type))

(defun bnf-mode-is-independent-multiplier? (v)
  (bnf-mode-base-predicate v 4 'independent-multiplier))

(defun bnf-mode-independent-multiplier-1 (v)
  (nth 1 v))

(defun bnf-mode-independent-multiplier-2 (v)
  (nth 2 v))

;;;

(defun bnf-mode-make-non-independent-multiplier ()
  (list 'non-independent-multiplier))


(defun bnf-mode-is-non-independent-multiplier? (v)
  (bnf-mode-base-predicate v 1 'non-independent-multiplier))

;;;

(defun bnf-mode-make-looking-for-independent-multiplier (v-rest column-start)
  "v-rest star-column-start"
  (list 'looking-for-independent-multiplier v-rest column-start))

(defun bnf-mode-is-looking-for-independent-multiplier? (v)
  (bnf-mode-base-predicate v 3 'looking-for-independent-multiplier))

(defun bnf-mode-looking-for-independent-multiplier-1 (v)
  (nth 1 v))

(defun bnf-mode-looking-for-independent-multiplier-2 (v)
  (nth 2 v))
  

;;;

(defun bnf-mode-make-multiplier-with-right-constant (v-rest column-start type)
  "v-rest star-column-start type"
  (list 'multiplier-with-right-constant v-rest column-start type))

(defun bnf-mode-is-multiplier-with-right-constant? (v)
  (bnf-mode-base-predicate v 4 'multiplier-with-right-constant))

(defun bnf-mode-multiplier-with-right-constant-1 (v)
  (nth 1 v))

(defun bnf-mode-multiplier-with-right-constant-2 (v)
  (nth 2 v))

;;;


(defun bnf-mode-make-multiplier-with-left-constant (v-rest column-start type)
  "v-rest star-column-start type"
  (list 'multiplier-with-left-constant v-rest column-start type))

(defun bnf-mode-is-multiplier-with-left-constant? (v)
  (bnf-mode-base-predicate v 4 'multiplier-with-left-constant))

(defun bnf-mode-multiplier-with-left-constant-1 (v)
  (nth 1 v))

(defun bnf-mode-multiplier-with-left-constant-2 (v)
  (nth 2 v))


;;;

(defun bnf-mode-make-multiplier-with-left-right-constant (v-rest column-start type)
  "v-rest star-column-start"
  (list 'multiplier-with-left-right-constant v-rest column-start type))

(defun bnf-mode-is-multiplier-with-left-right-constant? (v)
  (bnf-mode-base-predicate v 4 'multiplier-with-left-right-constant))

(defun bnf-mode-multiplier-with-left-right-constant-1 (v)
  (nth 1 v))

(defun bnf-mode-multiplier-with-left-right-constant-2 (v)
  (nth 2 v))



;; returns the offset where the star begins
(defun bnf-mode-is-str-independent-multiplier? (v)
  (bnf-mode-fold-left-break-before
   v
   (lambda (bnf-mode-x rest)
     (cond


     ;; "a" "*" "a"
      ((and (bnf-mode-is-str-constant? bnf-mode-x)
	    (or (bnf-mode-is-str-star? (car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest)))) 
		(bnf-mode-is-str-plus? (car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest)))))
	    (bnf-mode-is-str-constant? (car (cdr (cdr (bnf-mode-looking-for-independent-multiplier-1 rest))))))
       (bnf-mode-make-multiplier-with-left-right-constant
	(bnf-mode-looking-for-independent-multiplier-1 rest)
	(+ 1 (bnf-mode-looking-for-independent-multiplier-2 rest))
	(car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest)))))


      ;; "*" "a"
      ((and (or (bnf-mode-is-str-star? bnf-mode-x) 
		(bnf-mode-is-str-plus? bnf-mode-x))
	    (bnf-mode-is-str-constant? (car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest)))))
       (bnf-mode-make-multiplier-with-right-constant
	(cdr (bnf-mode-independent-multiplier-1 rest))
	(+ 1 (bnf-mode-independent-multiplier-2 rest))
	bnf-mode-x))

      ;; "a" "*"
      ((and (or (bnf-mode-is-str-star? (car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest)))) 
		(bnf-mode-is-str-plus? (car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest)))))
	    (bnf-mode-is-str-constant? bnf-mode-x))
       (bnf-mode-make-multiplier-with-left-constant
	(bnf-mode-independent-multiplier-1 rest)
	(+ 1 (bnf-mode-independent-multiplier-2 rest))
	(car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest)))))



      ;; "*" or "*" " "
      ((and (or (bnf-mode-is-str-star? bnf-mode-x) 
		(bnf-mode-is-str-plus? bnf-mode-x))
	    (or (null (car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest))))
		(bnf-mode-is-str-white-space? (car (cdr (bnf-mode-looking-for-independent-multiplier-1 rest))))))
       (bnf-mode-make-independent-multiplier
	(cdr (bnf-mode-independent-multiplier-1 rest))
	(+ 1 (bnf-mode-independent-multiplier-2 rest))
	bnf-mode-x))

      ;; " " "*"
      ((bnf-mode-is-str-white-space? bnf-mode-x)
       (if (bnf-mode-is-looking-for-independent-multiplier? rest)
	   (bnf-mode-make-looking-for-independent-multiplier
	    (cdr (bnf-mode-looking-for-independent-multiplier-1 rest))
	    (+ 1 (bnf-mode-looking-for-independent-multiplier-2 rest)))
	 rest))
      

      (t
       (bnf-mode-make-non-independent-multiplier))))

   (lambda () (bnf-mode-make-looking-for-independent-multiplier v -1))
   (lambda (bnf-mode-x rest)
     (or (bnf-mode-is-non-independent-multiplier? rest)
	 (bnf-mode-is-independent-multiplier? rest)
	 (bnf-mode-is-multiplier-with-right-constant? rest)
	 (bnf-mode-is-multiplier-with-left-constant? rest)
	 (bnf-mode-is-multiplier-with-left-right-constant? rest)))))




;; A star is only a kleene star if it is associated with something
;; that is a paren and does not stand alone.
(defun bnf-mode-is-str-multiplier? (v)
  )


;; A context with a multiplier (star or plus) where it is not
;; interpreted as a grammatical multiplier.
(defun bnf-mode-is-str-constant-multiplier? (v)
  (let ((r (bnf-mode-is-str-independent-multiplier? v)))
    (if (and (not (bnf-mode-is-non-independent-multiplier? r))
	     (not (bnf-mode-is-looking-for-independent-multiplier? r)))
	r
      nil)))

(bnf-mode-is-str-constant-multiplier?
 '("a" "a" "*"))
  
(bnf-mode-is-str-constant-multiplier?
 '("a" "*"))


(bnf-mode-is-str-independent-multiplier? 
 '("a" "*" "a"))

(bnf-mode-is-str-independent-multiplier? 
 '("*"))


(defun bnf-mode-bnf-handle-constant-acc (acc constant column-number)
  (if (> (length constant) 0)
      (setq acc (bnf-mode-extend-parsed-with-constant
		 acc
		 (bnf-mode-make-constant
		  (bnf-mode-make-bnf-location
		   (- column-number (length constant))
		   column-number)
		  (intern (bnf-mode-trim-string constant)))))
    acc))


(defun bnf-mode-bnf-parse (v column-number_ member-context paren-type)
  (let ((acc '())
	(members '())
	(r nil)
	(constant "")
	(group-constant "")
	(r nil)
	(column-number-group-begin 0)
	(column-number column-number_))
      (while v
	(cond
	   

	 ;; <a> ::= a | b
	 ;; <b> ::= c
	 ((bnf-mode-is-str-define? v column-number) 
	  (let ((define-group (bnf-mode-is-str-define? v column-number)))
	    ;; At the end of a set of members (when the next group is detected)
	    (if member-context
		(progn
		  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
		  (setq constant "")

		  (setq members (bnf-mode-combine-two-members members acc))
		  (setq r (bnf-mode-make-bnf-parse-remainder 
			   members 
			   v 
			   (bnf-mode-bnf-location-2 (bnf-mode-bnf-location-from-value members))
			   column-number))
		  (setq v nil)
		  (setq acc nil))

	      ;; type-of members: bnf-parse-remainder
	      (let ((members (bnf-mode-bnf-parse 
			      (bnf-mode-bnf-parse-remainder-2 define-group) 
			      (bnf-mode-bnf-parse-remainder-4 define-group)
			      t
			      paren-type)))

		(setq acc (bnf-mode-combine-two-groups
			   acc 
			   (bnf-mode-make-group
			    (bnf-mode-make-bnf-location
			     (bnf-mode-bnf-parse-remainder-3 define-group)
			     (bnf-mode-bnf-location-2 
			      (bnf-mode-bnf-location-from-value
			       (bnf-mode-bnf-parse-remainder-1 members))))
			    (intern (bnf-mode-bnf-parse-remainder-1 define-group))
			    (bnf-mode-bnf-parse-remainder-1 members)
			    )))

		(setq constant "")
		(setq column-number (bnf-mode-bnf-parse-remainder-4 members))
		(setq v (bnf-mode-bnf-parse-remainder-2 members))))))


       ((bnf-mode-is-str-comment? (car v))
	(setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	(setq constant "")

	(let ((comment-res (bnf-mode-sexp-handle-comment v)))
	  (setq v (bnf-mode-comment-remainder-1 comment-res))
	  (setq column-number (+ column-number (bnf-mode-comment-remainder-2 comment-res)))))

       ;; ((bnf-mode-is-str-quote? (car v))
       ;; 	(handle-constant)
       ;; 	(setq column-number (+ 1 column-number))
       ;; 	(setq constant (car v))
       ;; 	(handle-constant)
       ;; 	(setq v (cdr v)))
       

       
       ((bnf-mode-is-str-string? v)
	(setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	(setq constant "")
       	(let ((r (bnf-mode-is-str-string? v)))
	  (setq column-number (+ column-number (bnf-mode-parse-remainder-3 r)))
	  (setq acc (bnf-mode-bnf-handle-constant-acc
		     acc
		     (bnf-mode-parse-remainder-1 r)
		     column-number))
	  (setq v (bnf-mode-parse-remainder-2 r))))


       ((bnf-mode-is-str-quote? (car v))
	(setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	(setq constant "")
	
       	(let ((quote-res (bnf-mode-handle-bnf-quote-context (cdr v) column-number 'bnf-mode-make-quote)))
       	  (setq v (bnf-mode-quote-context-remainder-1 quote-res))
       	  (setq acc (bnf-mode-extend-parsed acc (bnf-mode-quote-context-remainder-2 quote-res)))
       	  (setq column-number (bnf-mode-quote-context-remainder-3 quote-res))))


       ((bnf-mode-is-str-comma? (car v))
       	(setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
       	(setq constant "")
	
       	(let ((quote-res (bnf-mode-handle-bnf-quote-context (cdr v) column-number 'bnf-mode-make-comma)))
       	  (setq v (bnf-mode-quote-context-remainder-1 quote-res))
       	  (setq acc (bnf-mode-extend-parsed acc (bnf-mode-quote-context-remainder-2 quote-res)))
       	  (setq column-number (bnf-mode-quote-context-remainder-3 quote-res))))


       ((bnf-mode-is-str-backward-quote? (car v))
       	(setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
       	(setq constant "")
	
       	(let ((quote-res (bnf-mode-handle-bnf-quote-context (cdr v) column-number 'bnf-mode-make-backward-quote)))
       	  (setq v (bnf-mode-quote-context-remainder-1 quote-res))
       	  (setq acc (bnf-mode-extend-parsed acc (bnf-mode-quote-context-remainder-2 quote-res)))
       	  (setq column-number (bnf-mode-quote-context-remainder-3 quote-res))))


	 ((bnf-mode-is-str-member? (car v))
	  (setq column-number (+ 1 column-number))
	  (setq members (bnf-mode-combine-two-members 
			 members 
			 acc))
	  (setq acc nil)
	  (setq constant "")
	  (setq v (cdr v)))

	 ;; Determins if the star is independent, i.e. stands alone or not
	 ((and (bnf-mode-is-str-constant-multiplier? v)
	       (or (null acc)
		   (bnf-mode-is-constant? acc)))

	  (let ((r (bnf-mode-is-str-constant-multiplier? v)))
	    (cond
	     
	     ((bnf-mode-is-multiplier-with-left-constant? r)
	      (setq constant 
		    (concat constant 
			    (car (bnf-mode-multiplier-with-left-constant-1 r))
			    (bnf-mode-multiplier-type-1 r)))
	      (setq v (cdr (cdr (bnf-mode-multiplier-with-left-constant-1 r))))
	      (setq column-number (+ 2 column-number (bnf-mode-multiplier-with-left-constant-2 r))))

	     ((bnf-mode-is-independent-multiplier? r)
	      (setq constant 
		    (concat constant 
			    (bnf-mode-multiplier-type-1 r)))
	      (setq v (bnf-mode-independent-multiplier-1 r))
	      (setq column-number (+ 1 column-number (bnf-mode-independent-multiplier-2 r))))
	      
	     ((bnf-mode-is-multiplier-with-right-constant? r)
	      (setq constant 
		    (concat constant 
			    (bnf-mode-multiplier-type-1 r)
			    (car (bnf-mode-multiplier-with-right-constant-1 r))))
	      (setq v (cdr (bnf-mode-multiplier-with-right-constant-1 r)))

	      (setq column-number (+ 2 column-number (bnf-mode-multiplier-with-right-constant-2 r))))

	     ((bnf-mode-is-multiplier-with-left-right-constant? r)
	      (setq constant 
		    (concat constant 
			    (car (bnf-mode-multiplier-with-left-right-constant-1 r))
			    (bnf-mode-multiplier-type-1 r)
			    (car (cdr (cdr (bnf-mode-multiplier-with-left-right-constant-1 r))))))
	      (setq v (cdr (cdr (cdr (bnf-mode-multiplier-with-left-right-constant-1 r)))))
	      (setq column-number (+ 3 column-number (bnf-mode-multiplier-with-left-right-constant-2 r)))))))




	 ((bnf-mode-is-str-star? (car v))
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (setq acc (bnf-mode-extend-parsed-star-plus acc 'bnf-mode-make-star))
	  (setq column-number (+ 1 column-number))
	  (setq v (cdr v)))
	    

	 ((bnf-mode-is-str-plus? (car v))
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (setq acc (bnf-mode-extend-parsed-star-plus acc 'bnf-mode-make-plus))
	  (setq column-number (+ 1 column-number))
	  (setq v (cdr v)))


	 ((bnf-mode-is-str-new-line? (car v))
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (setq column-number (+ 1 column-number))
	  (setq v (cdr v)))

	 ((bnf-mode-is-str-paren-begin? (car v))
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (let* ((paren-type (bnf-mode-derive-paren-type (car v)))
		 (inner (bnf-mode-bnf-parse (cdr v)
				   (+ 1 column-number)
				   t
				   paren-type))
		 (inner-value (if (bnf-mode-is-member? (bnf-mode-bnf-parse-remainder-1 inner))
				  (bnf-mode-member-1 (bnf-mode-bnf-parse-remainder-1 inner))
				(bnf-mode-bnf-parse-remainder-1 inner))))
	    (setq acc (bnf-mode-extend-parsed 
		       acc 
		       (bnf-mode-make-paren
			(bnf-mode-make-bnf-location
			 column-number
			 (bnf-mode-bnf-parse-remainder-4 inner))
			(bnf-mode-derive-paren-type (car v))
			inner-value)))
	    (setq v (bnf-mode-bnf-parse-remainder-2 inner))
	    (setq column-number (bnf-mode-bnf-parse-remainder-4 inner))
	    ))


	 ((bnf-mode-is-str-paren-end? (car v))	
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (if (not (equal (bnf-mode-derive-paren-type (car v)) paren-type))
	      (error "BNF error, open and close parenthese must be the same: %s %s" (car v) paren-type))
	  (if (bnf-mode-is-value? acc)
	      (setq r (bnf-mode-make-bnf-parse-remainder (list acc) (cdr v) -1 (+ 1 column-number)))
	    (setq r (bnf-mode-make-bnf-parse-remainder acc (cdr v) -1 (+ 1 column-number))))
	  (setq v nil))

	 ((bnf-mode-is-group-name-ref? v column-number)
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (let ((group-name (bnf-mode-is-group-name-ref? v column-number)))
	    (setq group-constant (concat "<" (bnf-mode-parse-remainder-1 group-name) ">"))
	    (if member-context
		(setq acc (bnf-mode-extend-parsed
			   acc
			   (bnf-mode-make-group-ref
			    (bnf-mode-make-bnf-location
			     column-number
			     (+ 2 (bnf-mode-parse-remainder-3 group-name)))
			    (intern group-constant)))))
			    

	    (setq column-number-group-begin column-number)
	    (setq column-number (+ 2 (bnf-mode-parse-remainder-3 group-name)))
	    (setq v (bnf-mode-parse-remainder-2 group-name))
	    ))	 

	 ((bnf-mode-is-str-arrow-begin? (car v))
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (setq column-number (+ 1 column-number))
	  (setq constant (concat constant (car v)))
	  (setq v (cdr v)))


	  

	 ;; ((bnf-mode-is-str-arrow-begin? (car v))
	 ;;  (handle-constant)
	 ;;  (let ((group-name (bnf-mode-bnf-parse-group-name-context (cdr v) column-number)))
	 ;;    (setq group-constant (concat "<" (bnf-mode-parse-remainder-1 group-name) ">"))
	 ;;    (if member-context
	 ;; 	(setq acc (bnf-mode-extend-parsed
	 ;; 		   acc
	 ;; 		   (bnf-mode-make-group-ref
	 ;; 		    (bnf-mode-make-bnf-location
	 ;; 		     column-number
	 ;; 		     (+ 2 (bnf-mode-parse-remainder-3 group-name)))
	 ;; 		    (intern group-constant)))))
			    

	 ;;    (setq column-number-group-begin column-number)
	 ;;    (setq column-number (+ 2 (bnf-mode-parse-remainder-3 group-name)))
	 ;;    (setq v (bnf-mode-parse-remainder-2 group-name))
	 ;;    ))

	 


	 ((bnf-mode-is-str-constant? (car v))
	  (setq column-number (+ 1 column-number))
	  (setq constant (concat constant (car v)))
	  (setq v (cdr v)))

	 ((bnf-mode-is-str-white-space? (car v))
	  (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
	  (setq constant "")

	  (setq constant "")
	  (setq column-number (+ 1 column-number))
	  (setq v (cdr v)))))

      (setq acc (bnf-mode-bnf-handle-constant-acc acc constant column-number))
      (setq constant "")
      
      (if (null r)
	  (if member-context
	      (let ((res (bnf-mode-combine-two-members members acc)))
		(bnf-mode-make-bnf-parse-remainder 
		 res 
		 v 
		 (bnf-mode-bnf-location-2 
		  (bnf-mode-bnf-location-from-value res)) 
		 column-number))
	    (bnf-mode-make-bnf-parse-remainder 
	     acc 
	     v 
	     (bnf-mode-bnf-location-2 
	      (bnf-mode-bnf-location-from-value acc)) 
	     column-number))
      	r)))


(defun bnf-mode-length-of-bnf-value (v)
  (cond
   
   ((null v)
    0)

   ((bnf-mode-is-constant? v)
    1)

   ((bnf-mode-is-member? v)
    (length (bnf-mode-member-1 v)))

   (t 
    (length v))
   ))

(provide 'bnf-parser)







(require 'bnf-util)
(require 'bnf-booleans)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  BUILD INS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The "normal" predicates only takes a sexp-value as input.
(defun bnf-mode-normal-buildin-productions ()
  '(; (<boolean> bnf-mode-bi-is-boolean?) boolean IS scheme-boolean
    (<boolean> bnf-mode-bi-is-scheme-boolean?) 
    (<scheme-boolean> bnf-mode-bi-is-scheme-boolean?) 
    (<integer> bnf-mode-bi-is-integer?)
    (<number> bnf-mode-bi-is-number?)
    (<natural-number> bnf-mode-bi-is-natural-number?)
    (<character> bnf-mode-bi-is-scheme-character?)
;    (<character> bnf-mode-bi-is-character?)
;    (<scheme-character> bnf-mode-bi-is-scheme-character?)
; For now character IS scheme-character
    (<string> bnf-mode-bi-is-string?)
    (<symbol> bnf-mode-bi-is-symbol?)		
    (<identifier> bnf-mode-bi-is-identifier?)
    (<nothing> bnf-mode-bi-is-nothing?)
))

(defun bnf-mode-is-reference-build-in-nothing? (ref)
  (equal '<nothing> (bnf-mode-group-ref-1 ref)))


;; Buildin productions that require a keyword list from the bnf as
;; input.
(defun bnf-mode-keyword-list-buildin-productions ()
  '((<variable> bnf-mode-bi-is-variable?)))

;;;

(defun bnf-mode-make-keyword-list-buildin (production f-check)
  (list 'keyword-list-buildin production f-check))

(defun bnf-mode-is-keyword-list-buildin? (v)
  (bnf-mode-base-predicate v 3 'keyword-list-buildin))

(defun bnf-mode-keyword-list-buildin-1 (v)
  (nth 1 v))

(defun bnf-mode-keyword-list-buildin-2 (v)
  (nth 2 v))

;;;

(defun bnf-mode-make-normal-buildin (production f-check)
  (list 'normal-buildin production f-check))

(defun bnf-mode-is-normal-buildin? (v)
  (bnf-mode-base-predicate v 3 'normal-buildin))

(defun bnf-mode-normal-buildin-1 (v)
  (nth 1 v))

(defun bnf-mode-normal-buildin-2 (v)
  (nth 2 v))

;;;

(defun bnf-mode-make-non-buildin ()
  (list 'non-buildin))

(defun bnf-mode-is-non-buildin? (v)
  (bnf-mode-base-predicate v 1 'non-buildin))

;;;


;; Used for lookup in a production list: 
;;list of list of group-ref * func of one arg
(defun bnf-mode-lookup-production (ps p)
  (bnf-mode-fold-left-break-at
   ps
   (lambda (bnf-mode-x rest)
     (if (equal (car bnf-mode-x) p)
	 bnf-mode-x
       rest))
   (lambda () nil)
   (lambda (bnf-mode-x rest)
     (equal (car bnf-mode-x) p))))


;; Assumes that the given value is a group-ref
(defun bnf-mode-is-build-in-production? (group-ref) 
  (let* ((look-up-normal-buildin
	 (bnf-mode-lookup-production 
	  (bnf-mode-normal-buildin-productions) 
	  group-ref))

	(lookup-keyword-list-buildin
	 (if look-up-normal-buildin
	     nil
	   (bnf-mode-lookup-production
	    (bnf-mode-keyword-list-buildin-productions)
	    group-ref))))

    (cond
     (look-up-normal-buildin
      (bnf-mode-make-normal-buildin 
       (nth 0 look-up-normal-buildin)
       (nth 1 look-up-normal-buildin)))
     
     (lookup-keyword-list-buildin
      (bnf-mode-make-keyword-list-buildin 
       (nth 0 lookup-keyword-list-buildin)
       (nth 1 lookup-keyword-list-buildin)))

     (t
      (bnf-mode-make-non-buildin)))))






;;;;;;;;;;;;;;;;;;; Error constructors for the buildin predicates ;;;;;;;;;;;;;;;

(defun bnf-mode-value-from-build-in-error (e)
  (nth 1 e))

;;;

(defun bnf-mode-make-error-build-in-nothing (v)
  (list 'error-build-in-nothing v '<nothing>))

(defun bnf-mode-is-error-build-in-nothing? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-nothing))


;;;


(defun bnf-mode-make-error-build-in-variable (v)
  (list 'error-build-in-variable v '<variable>))

(defun bnf-mode-is-error-build-in-variable? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-variable))

;;;

(defun bnf-mode-make-error-build-in-identifier (v)
  (list 'error-build-in-identifier v '<identifier>))

(defun bnf-mode-is-error-build-in-identifier? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-identifier))


;;;

(defun bnf-mode-make-error-build-in-natural-number (v)
  (list 'error-build-in-natural-number v '<natural-number>))

(defun bnf-mode-is-error-build-in-natural-number? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-natural-number))


;;;;;;;;;;;;;;;;


(defun bnf-mode-make-error-build-in-number (v)
  (list 'error-build-in-number v '<number> ))

(defun bnf-mode-is-error-build-in-number? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-number))

;;;;;;;;;;;;;;;;

(defun bnf-mode-make-error-build-in-integer (v)
  (list 'error-build-in-integer  v '<integer>))

(defun bnf-mode-is-error-build-in-integer? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-integer))

;;;;;;;;;;;;;;;;

(defun bnf-mode-make-error-build-in-boolean (v)
  (list 'error-build-in-boolean  v '<boolean>))

(defun bnf-mode-is-error-build-in-boolean? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-boolean))


;;;;;;;;;;;;;;;;


(defun bnf-mode-make-error-build-in-character (v)
  (list 'error-build-in-character  v '<character>))


(defun bnf-mode-is-error-build-in-character? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-character))


;;;;;;;;;;;;;;;;


(defun bnf-mode-make-error-build-in-scheme-character (v)
  (list 'error-build-in-scheme-character  v '<scheme-character>))


(defun bnf-mode-is-error-build-in-scheme-character? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-scheme-character))

;;;;;;;;;;;;;;;;


(defun bnf-mode-make-error-build-in-character-too-long (v)
  (list 'error-build-in-character-too-long v '<character>))

(defun bnf-mode-make-error-build-in-string (v)
  (list 'error-build-in-string v '<string>))

(defun bnf-mode-is-error-build-in-string? (v)
  (bnf-mode-base-predicate v 3 'error-build-in-string))


(defun bnf-mode-make-error-build-in-symbol (v)
  (list 'error-build-in-symbol v))

(defun bnf-mode-make-error-build-in-symbol-illegal-char (v)
  (list 'error-build-in-symbol-illegal-char v '"\]\[(){},'`;#|\\"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Build in predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun bnf-mode-bi-is-nothing? (v)
  (bnf-mode-boolean2boolean 
   (null v)
   (bnf-mode-make-error-build-in-nothing v)))
  

;; <boolean> ::= bnf-mode-true | bnf-mode-false
(defun bnf-mode-bi-is-boolean? (v)
  (bnf-mode-boolean2boolean 
   (and (not (null v)) 
	(bnf-mode-is-constant? v) 
	(member (bnf-mode-constant-1 v) '(bnf-mode-true bnf-mode-false)))
   (bnf-mode-make-error-build-in-boolean v)))


(defun bnf-mode-bi-is-scheme-boolean? (v)
  (bnf-mode-boolean2boolean 
   (and (not (null v)) 
	(bnf-mode-is-constant? v) 
	(member (bnf-mode-constant-1 v) '(\#t \#f)))
   (bnf-mode-make-error-build-in-boolean v)))




;; <integer> ::= ... | -4 | -3 | -2 | -1 | 0 | 1 | 2 | 3 | 4 | ...
(defun bnf-mode-str-is-numeric? (str)
  (let ((lst (string-to-list str)) (r t))
    (while lst
      (if (and (<= (car lst) 57)
	       (>= (car lst) 48))
	  (setq lst (cdr lst))
	(progn
	  (setq r nil)
	  (setq lst nil))))
    r))

(bnf-mode-str-is-numeric? "false")
(bnf-mode-bnf-mode-string-to-list "false")


(defun bnf-mode-constant-to-string (v)
  (symbol-name (bnf-mode-constant-1 v)))

;; Assumes that v is a valid integer, since the string-to-int func
;; ignores chars.
(defun bnf-mode-constant-to-integer (v)
  (string-to-number (bnf-mode-constant-to-string v)))


(defun bnf-mode-is-constant-integer? (v)
  (bnf-mode-str-is-numeric? 
   (symbol-name 
    (bnf-mode-constant-1 v))))

(defun bnf-mode-bi-is-integer? (v)
  (bnf-mode-boolean2boolean 
   (and (not (null v))
	(bnf-mode-is-constant? v) 
	(bnf-mode-is-constant-integer? v))
   (bnf-mode-make-error-build-in-integer v)))

;; <natural-number> ::= 0 | 1 | 2 | 3 | 4 ...
(defun bnf-mode-bi-is-natural-number? (v)
  (bnf-mode-boolean2boolean
   (and (not (null v))
	(bnf-mode-is-constant? v)
	(bnf-mode-is-true? (bnf-mode-bi-is-integer? v))
	(>= (bnf-mode-constant-to-integer v) 0))
   (bnf-mode-make-error-build-in-natural-number v)))

;; Like integer
(defun bnf-mode-bi-is-number? (v)
  (bnf-mode-boolean2boolean
   (and (not (null v))
	(bnf-mode-is-constant? v) 
	(bnf-mode-is-constant-integer? v))
   (bnf-mode-make-error-build-in-number v)))

;; <character> ::= 0 | 1 | 2 ... | a | b | c ... 궧 | 긗 | 긘
(defun bnf-mode-bi-is-character? (v)
  (cond
   ((and (bnf-mode-is-constant? v) 
	 (= 1 (length (bnf-mode-constant-to-string v))))
    (bnf-mode-boolean2boolean
     (characterp (string-to-char (bnf-mode-constant-to-string v)))
     (bnf-mode-make-error-build-in-character v)))

   ((and (bnf-mode-is-constant? v) 
	 (> (length (bnf-mode-constant-to-string v)) 1))
    (bnf-mode-false 
     (bnf-mode-make-error-build-in-character-too-long v)))

   (t
    (bnf-mode-false
     (bnf-mode-make-error-build-in-character v)))))

;; A character check for Scheme, examples:
;; #\c #\a, #\space
(defun bnf-mode-bi-is-scheme-character? (v)
  (if (not (bnf-mode-is-constant? v))
      (bnf-mode-false
       (bnf-mode-make-error-build-in-scheme-character v))
    (let ((char-str (bnf-mode-constant-to-string v)))
      (cond

       ;; default case #\a
       ((= 3 (length char-str))
	(bnf-mode-boolean2boolean
	 (and (equal (substring char-str 0 1) "#")
	      (equal (substring char-str 1 2) "\\"))
	 (bnf-mode-make-error-build-in-scheme-character v)))

       ;; special case for space.
       ;; #\space
       ((= 7 (length char-str))
	(bnf-mode-boolean2boolean
	 (equal char-str  "#\\space")
	 (bnf-mode-make-error-build-in-scheme-character v)))
	
     (t
      (bnf-mode-false
       (bnf-mode-make-error-build-in-scheme-character v)))))))


;; An identifier is written with letters, digits, and most funky
;; characters on your keyboard (-, *, ?, !, etc.), excluding quote,
;; backquote, and comma as well as # in bnf-mode-first position. You can Read
;; The Friendly Manual (the 4 bnf-mode-first paragraphs) for detail, but this
;; level of detail won’t be at the exam.
;; (defun bnf-mode-bi-is-identifier? (v)
;;   (if (bnf-mode-is-constant? v)
;;       (let ((str (bnf-mode-constant-to-string v)) 
;; 	    (illegal-chars "[\"´,]"))
;; 	(bnf-mode-boolean2boolean
;; 	 (and (not (string-match illegal-chars str))
;; 	      (not (string= "#" (substring str 0 1))))
;; 	 (bnf-mode-make-error-build-in-identifier v)))
;;     (bnf-mode-false (bnf-mode-make-error-build-in-identifier v))))
;; 
(defun bnf-mode-bi-is-identifier? (v)
  (bnf-mode-bi-is-identifier-or-symbol?
   v 
   (bnf-mode-make-error-build-in-identifier v)))

(defun bnf-mode-bi-is-symbol? (v)
  (bnf-mode-bi-is-identifier-or-symbol? 
   v 
   (bnf-mode-make-error-build-in-symbol v)))


;; A symbol is identical to an identifier, but differnet errors should
;; be reported.
(defun bnf-mode-bi-is-identifier-or-symbol? (v error)
  (if (bnf-mode-is-constant? v)
      (let ((str (bnf-mode-constant-to-string v)) 
	    (illegal-chars "[\"`,]"))
	(bnf-mode-boolean2boolean
	 (and (not (string-match illegal-chars str))
	      (not (string= "#" (substring str 0 1))))
	 error))
    (bnf-mode-false error)))



;; Some thing that is an identifier but not a keyword in the bnf
(defun bnf-mode-bi-is-variable? (v bnf-keywords)
  (if (bnf-mode-is-constant? v)
      (bnf-mode-boolean2boolean
       (and (not (member (bnf-mode-constant-1 v) bnf-keywords))
	    (bnf-mode-is-true? (bnf-mode-bi-is-identifier? v)))
       (bnf-mode-make-error-build-in-variable v))
    (bnf-mode-false (bnf-mode-make-error-build-in-variable v))))



;; A tring must start and end with a double quote.
(defun bnf-mode-bi-is-string? (v)
  (if (bnf-mode-is-constant? v)
      (let ((str (bnf-mode-constant-to-string v)))
	(bnf-mode-boolean2boolean
	 (and (<= 2 (length str))
	      (equal "\"" (substring str 0 1))
	      (equal "\"" (substring str (- (length str) 1))))
	 (bnf-mode-make-error-build-in-string v)))
    (bnf-mode-false (bnf-mode-make-error-build-in-string v))))


;; Helper
(defun bnf-mode-boolean2boolean (tf v)
  (if tf
      (bnf-mode-true)
    (bnf-mode-false v)))

(defun bnf-mode-is-build-in-error? (v)
  (or (bnf-mode-is-error-build-in-nothing? v)
      (bnf-mode-is-error-build-in-variable? v)
      (bnf-mode-is-error-build-in-identifier? v)
      (bnf-mode-is-error-build-in-boolean? v)
      (bnf-mode-is-error-build-in-character? v)
      (bnf-mode-is-error-build-in-integer? v)
      (bnf-mode-is-error-build-in-natural-number? v)
      (bnf-mode-is-error-build-in-number? v)
      (bnf-mode-is-error-build-in-string? v)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  / BUILD INS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A cache for the bnf keywords
 (defvar bnf-mode-key-words nil)

;; (bnf-mode-reset-bnf-key-words (bnf-mode-bnf-parse-main "<a> ::= (one tow)"))
(defun bnf-mode-reset-bnf-key-words (bnf)
  (setq bnf-mode-key-words 
	(bnf-mode-key-words-list-from-bnf-cps-tramp-defunc-main bnf)))


;; (defun bnf-mode-reset-bnf-key-words (bnf)
;;   (setq bnf-mode-key-words 
;; 	(bnf-mode-key-words-list-from-bnf bnf '())))





;; (defun bnf-mode-key-words-list-from-bnf-main (bnf)
;;   (if bnf-mode-key-words
;;       bnf-mode-key-words
;;     (let ((kw 
;; 	   (bnf-mode-key-words-list-from-bnf bnf  '())))
;;       (setq bnf-mode-key-words kw)
;;       kw)))


;; (bnf-mode-key-words-list-from-bnf-main (bnf-mode-bnf-parse-main "<a> ::= (one two)"))
(defun bnf-mode-key-words-list-from-bnf-main (bnf)
  (if bnf-mode-key-words
      bnf-mode-key-words
    (let ((kw 
	   (bnf-mode-key-words-list-from-bnf-cps-tramp-defunc-main bnf)))
      (setq bnf-mode-key-words kw)
      kw)))

;; CPS + tramp + defunc transformed verson 

(defun bnf-mode-array-base-predicate (v len name)
  (and (eq (aref v 0) name)
       (eq len (length v))))



(defun bnf-mode-C0 ()
  (vector 'bnf-mode-C0))

(defun bnf-mode-C0? (v)
  (bnf-mode-array-base-predicate v 1 'bnf-mode-C0))

;;;

(defun bnf-mode-T1 (k bnf)
  (vector 'bnf-mode-T1 k bnf))

(defun bnf-mode-T1? (v)
  (bnf-mode-array-base-predicate v 3 'bnf-mode-T1))

(defun bnf-mode-T1-1 (v)
  (aref v 1))

(defun bnf-mode-T1-2 (v)
  (aref v 2))

;;;

(defun bnf-mode-T2 (k)
  (vector 'bnf-mode-T2 k))

(defun bnf-mode-T2? (v)
  (bnf-mode-array-base-predicate v 2 'bnf-mode-T2))

(defun bnf-mode-T2-1 (v)
  (aref v 1))

;;;

(defun bnf-mode-T3 (k bnf)
  (vector 'bnf-mode-T3 k bnf))

(defun bnf-mode-T3? (v)
  (bnf-mode-array-base-predicate v 3 'bnf-mode-T3))

(defun bnf-mode-T3-1 (v)
  (aref v 1))

(defun bnf-mode-T3-2 (v)
  (aref v 2))

;;;

(defun bnf-mode-T4 (k bnf-mode-x y)
  (vector 'bnf-mode-T4 k bnf-mode-x y))

(defun bnf-mode-T4? (v)
  (bnf-mode-array-base-predicate v 4 'bnf-mode-T4))

(defun bnf-mode-T4-1 (v)
  (aref v 1))

(defun bnf-mode-T4-2 (v)
  (aref v 2))

(defun bnf-mode-T4-3 (v)
  (aref v 3))

;;;

(defun bnf-mode-C5 (k bnf)
  (vector 'bnf-mode-C5 k bnf))

(defun bnf-mode-C5? (v)
  (bnf-mode-array-base-predicate v 3 'bnf-mode-C5))

(defun bnf-mode-C5-1 (v)
  (aref v 1))

(defun bnf-mode-C5-2 (v)
  (aref v 2))

;;;



(defun bnf-mode-T6 (bnf-cdr k bnf-mode-x)
  (vector 'bnf-mode-T6 bnf-cdr k bnf-mode-x))

(defun bnf-mode-T6? (v)
  (bnf-mode-array-base-predicate v 4 'bnf-mode-T6))

(defun bnf-mode-T6-1 (v)
  (aref v 1))

(defun bnf-mode-T6-2 (v)
  (aref v 2))

(defun bnf-mode-T6-3 (v)
  (aref v 3))

;;;


(defun bnf-mode-T7 (k bnf-mode-x)
  (vector 'bnf-mode-T7 k bnf-mode-x))

(defun bnf-mode-T7? (v)
  (bnf-mode-array-base-predicate v 3 'bnf-mode-T7))

(defun bnf-mode-T7-1 (v)
  (aref v 1))

(defun bnf-mode-T7-2 (v)
  (aref v 2))

;;;

(defun bnf-mode-C8 (bnf-cdr k)
  (vector  'bnf-mode-C8 bnf-cdr k))

(defun bnf-mode-C8? (v)
  (bnf-mode-array-base-predicate v 3 'bnf-mode-C8))

(defun bnf-mode-C8-1 (v)
  (aref v 1))

(defun bnf-mode-C8-2 (v)
  (aref v 2))

;;;

(defun bnf-mode-T9 (car-bnf cdr-bnf k)
  (vector 'bnf-mode-T9 car-bnf cdr-bnf k))

(defun bnf-mode-T9? (v)
  (bnf-mode-array-base-predicate v 4 'bnf-mode-T9))

(defun bnf-mode-T9-1 (v)
  (aref v 1))

(defun bnf-mode-T9-2 (v)
  (aref v 2))

(defun bnf-mode-T9-3 (v)
  (aref v 3))

;; Used to apply the defunctionalized continuations
(defun bnf-mode-apply_cont (v bnf-mode-x)
  (cond
   
   ((bnf-mode-C0? v)
    (bnf-mode-trampoline-stop bnf-mode-x))

   ((bnf-mode-C5? v)
    (bnf-mode-trampoline-continue
     (bnf-mode-T4 (bnf-mode-C5-1 v) (bnf-mode-C5-2 v) bnf-mode-x)))

   ((bnf-mode-C8? v)
    (if (bnf-mode-C8-1 v)
	(bnf-mode-trampoline-continue
	 (bnf-mode-T6 (bnf-mode-C8-1 v) (bnf-mode-C8-2 v) bnf-mode-x))
      (bnf-mode-trampoline-continue
       (bnf-mode-T7 (bnf-mode-C8-2 v) bnf-mode-x))))

))

;; Used to apply the defunctionalized thunks
(defun bnf-mode-apply_trampoline_thunk (v)
  (cond

   ((bnf-mode-T1? v)
    (bnf-mode-apply_cont
     (bnf-mode-T1-1 v) 
     (list (bnf-mode-constant-1 (bnf-mode-T1-2 v)))))

   ((bnf-mode-T2? v)
    (bnf-mode-apply_cont (bnf-mode-T2-1 v) '()))

   ((bnf-mode-T3? v)
    (bnf-mode-key-words-list-from-bnf-cps-tramp-defunc
     (bnf-mode-T3-1 v)
     (bnf-mode-T3-2 v)))

   ((bnf-mode-T4? v)
     (bnf-mode-apply_cont 
      (bnf-mode-T4-1 v)
      (bnf-mode-set-append (bnf-mode-T4-2 v) (bnf-mode-T4-3 v))))

   ((bnf-mode-T6? v)
    (bnf-mode-key-words-list-from-bnf-cps-tramp-defunc
     (bnf-mode-T6-1 v)
     (bnf-mode-C5 (bnf-mode-T6-2 v) (bnf-mode-T6-3 v))))

   ((bnf-mode-T7? v)
    (bnf-mode-apply_cont 
     (bnf-mode-T7-1 v) 
     (bnf-mode-T7-2 v)))

   ((bnf-mode-T9? v)
    (bnf-mode-key-words-list-from-bnf-cps-tramp-defunc
     (bnf-mode-T9-1 v)
     (bnf-mode-C8 (bnf-mode-T9-2 v) (bnf-mode-T9-3 v))))

))



(defun bnf-mode-key-words-list-from-bnf-cps-tramp-defunc (bnf f)
  (cond

   ((bnf-mode-is-constant? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T1 f bnf)))

   ((bnf-mode-is-group-ref? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T2 f)))

   ((bnf-mode-is-bnf? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T3 (bnf-mode-bnf-1 bnf) f)))

   ((bnf-mode-is-groups? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T3 (bnf-mode-groups-1 bnf) f)))
 
   ((bnf-mode-is-group? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T3 (bnf-mode-group-2 bnf) f)))
   
   ((bnf-mode-is-members? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T3 (bnf-mode-members-1 bnf) f)))
   
   ((bnf-mode-is-member? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T3 (bnf-mode-member-1 bnf) f)))

   ((bnf-mode-is-paren? bnf)
    (if (bnf-mode-paren-2 bnf)
	(bnf-mode-trampoline-continue
	 (bnf-mode-T3 (bnf-mode-paren-2 bnf) f))
      (bnf-mode-trampoline-continue
       (bnf-mode-T2 f))))

   ((bnf-mode-is-star? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T3 (bnf-mode-star-1 bnf) f)))

   ((bnf-mode-is-plus? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T3 (bnf-mode-plus-1 bnf) f)))

   ((bnf-mode-is-value-list? bnf)
    (bnf-mode-trampoline-continue
     (bnf-mode-T9 (car bnf) (cdr bnf) f)))
      
))



(defun bnf-mode-trampoline-defunc (v)
  (cond

   ((bnf-mode-is-trampoline-stop? v)
    (bnf-mode-trampoline-stop-1 v))

   ((bnf-mode-is-trampoline-continue? v)
    (setq res (bnf-mode-apply_trampoline_thunk (bnf-mode-trampoline-continue-1 v)))
    (while (not (bnf-mode-is-trampoline-stop? res))
      (setq res (bnf-mode-apply_trampoline_thunk (bnf-mode-trampoline-continue-1 res))))
    (bnf-mode-trampoline-stop-1 res))))





(defun bnf-mode-key-words-list-from-bnf-cps-tramp-defunc-main (bnf)
  (bnf-mode-trampoline-defunc
   (bnf-mode-key-words-list-from-bnf-cps-tramp-defunc
    bnf
    (bnf-mode-C0))))













;; ;; A set append
;; (defun bnf-mode-append-key-word (lst word)
;;   (if (member word lst)
;;       lst
;;     (append lst  (list word))))


;; ;; ;; A set union
;; (defun bnf-mode-append-key-word-list (left right)
;;   (bnf-mode-fold-left
;;    right
;;    (lambda (bnf-mode-x rest)
;;      (bnf-mode-append-key-word rest bnf-mode-x))
;;    (lambda () left)))


;; (defun bnf-mode-key-words-list-from-bnf (bnf acc)
;;   (cond
   
;;    ((bnf-mode-is-bnf? bnf)
;;     (bnf-mode-key-words-list-from-bnf (bnf-mode-bnf-1 bnf) acc))

;;    ((bnf-mode-is-groups? bnf)
;;     (bnf-mode-fold-left
;;      (bnf-mode-groups-1 bnf)
;;      (lambda (bnf-mode-x rest)
;;        (bnf-mode-append-key-word-list rest (bnf-mode-key-words-list-from-bnf bnf-mode-x acc)))
;;      (lambda () nil)))

;;    ((bnf-mode-is-group? bnf)
;;     (bnf-mode-key-words-list-from-bnf (bnf-mode-group-2 bnf) acc))

;;    ((bnf-mode-is-members? bnf)
;;     (bnf-mode-fold-left
;;      (bnf-mode-members-1 bnf)
;;      (lambda (bnf-mode-x rest)
;;        (bnf-mode-append-key-word-list rest (bnf-mode-key-words-list-from-bnf bnf-mode-x acc)))
;;      (lambda () nil)))


;;    ((bnf-mode-is-member? bnf)
;;     (bnf-mode-fold-left
;;      (bnf-mode-members-1 bnf)
;;      (lambda (bnf-mode-x rest)
;;        (bnf-mode-append-key-word-list rest (bnf-mode-key-words-list-from-bnf bnf-mode-x acc)))
;;      (lambda () nil)))

;;    ((bnf-mode-is-constant? bnf)
;;     (bnf-mode-append-key-word acc (bnf-mode-constant-1 bnf)))

;;    ((bnf-mode-is-paren? bnf)
;;     (bnf-mode-fold-left
;;      (bnf-mode-paren-2 bnf)
;;      (lambda (bnf-mode-x rest)
;;        (bnf-mode-append-key-word-list rest (bnf-mode-key-words-list-from-bnf bnf-mode-x acc)))
;;      (lambda () nil)))


;;    ((bnf-mode-is-star? bnf)
;;     (bnf-mode-fold-left
;;      (bnf-mode-star-1 bnf)
;;      (lambda (bnf-mode-x rest)
;;        (bnf-mode-append-key-word-list rest (bnf-mode-key-words-list-from-bnf bnf-mode-x acc)))
;;      (lambda () nil)))



;;    ((bnf-mode-is-plus? bnf)
;;     (bnf-mode-fold-left
;;      (bnf-mode-plus-1 bnf)
;;      (lambda (bnf-mode-x rest)
;;        (bnf-mode-append-key-word-list rest (bnf-mode-key-words-list-from-bnf bnf-mode-x acc)))
;;      (lambda () nil)))

;;    (t acc)))

(provide 'bnf-key-words)
;;; -*- lexical-binding: t -*-

(require 'bnf-booleans)
(require 'data-structure)
(require 'bnf-util)
(require 'bnf-parser)
(require 'sexp-parser)
(require 'bnf-key-words)


;;;;;;;;;;;;;;;;;;; Check functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bnf-mode-check-arity (member-list v)
  (if (equal (bnf-mode-length-of-bnf-value member-list)
	     (bnf-mode-length-of-sexp-value v))
      (bnf-mode-true)
    (bnf-mode-false 
     (bnf-mode-make-error-arity
      ;; If the bnf-member-list is longer than the input,
      ;; point to the end of the bnf-member-list where input is missing.
      (if (> (bnf-mode-length-of-bnf-value member-list)
	     (bnf-mode-length-of-sexp-value v))
	  (bnf-mode-n-last-of-list
	   (- (bnf-mode-length-of-bnf-value member-list)
	      (bnf-mode-length-of-sexp-value v))
	   v)
	v)
 
      ;; bnf location
      (if (> (bnf-mode-length-of-bnf-value member-list)
	     (bnf-mode-length-of-sexp-value v))
	  (let ((lst 
		 (bnf-mode-n-last-of-list
		  (- (bnf-mode-length-of-bnf-value member-list)
		     (bnf-mode-length-of-sexp-value v))
		  member-list)))
	    (bnf-mode-extend-bnf-location 
	     (bnf-mode-bnf-location-from-value (car lst))
	     (bnf-mode-bnf-location-from-value (bnf-mode-last-element-of-list lst))))
	     
	(bnf-mode-extend-bnf-location
	 (bnf-mode-bnf-location-from-value 
	  (car member-list))
	 (bnf-mode-bnf-location-from-value 
	  (bnf-mode-last-element-of-list member-list))))
      
      (bnf-mode-length-of-bnf-value member-list)
      (bnf-mode-length-of-sexp-value v)))))

;; Returns the least arity of a BNF
;; (abc {<ref>}*) = 1
;; (abc {<ref>}+) = 2
;; (abc {<ref>}* def) = 2
(defun bnf-mode-shortest-bnf-length (member-list)
  (bnf-mode-fold-left
   member-list
   (lambda (bnf-mode-x rest)
     (cond
      ((bnf-mode-is-star? bnf-mode-x) rest)
      ((bnf-mode-is-plus? bnf-mode-x) (+ 1 rest))
      (t (+ 1 rest))))
   (lambda () 0)))


(defun bnf-mode-check-arity-with-multiplier (member-list v)
  (if (<= (bnf-mode-shortest-bnf-length member-list)
	  (bnf-mode-length-of-sexp-value v))
      (bnf-mode-true)
    (bnf-mode-false 
     (bnf-mode-make-error-arity-with-multiplier
      v
      (bnf-mode-bnf-location-from-value 
       (car member-list))
      (bnf-mode-shortest-bnf-length member-list)
      (bnf-mode-length-of-sexp-value v)))))
   	      


(defun bnf-mode-check-paren-exists (bnf-paren v)
  (if (bnf-mode-is-syntax-paren? v)
      (bnf-mode-true)
    (bnf-mode-false
     (bnf-mode-make-error-missing-paren v (bnf-mode-bnf-location-from-value bnf-paren)))))

(defun bnf-mode-check-paren-type (bnf-paren v)
  (cond
   ((and (not (equal (bnf-mode-syntax-paren-1 v) (bnf-mode-paren-1 bnf-paren)))
	 (not (equal (bnf-mode-syntax-paren-3 v) (bnf-mode-paren-1 bnf-paren))))
    (bnf-mode-false (bnf-mode-make-error-wrong-paren-type v (bnf-mode-bnf-location-from-value bnf-paren))))
   
   ((not (equal (bnf-mode-syntax-paren-1 v) (bnf-mode-paren-1 bnf-paren)))
    (bnf-mode-false 
     (bnf-mode-make-error-paren-type-mismatch-left 
      v 
      (bnf-mode-make-bnf-location
       (bnf-mode-bnf-location-1 (bnf-mode-bnf-location-from-value bnf-paren))
       (+ 1 (bnf-mode-bnf-location-1 (bnf-mode-bnf-location-from-value bnf-paren)))))))

   ((not (equal (bnf-mode-syntax-paren-3 v) (bnf-mode-paren-1 bnf-paren)))
    (bnf-mode-false
     (bnf-mode-make-error-paren-type-mismatch-right 
      v 
      (bnf-mode-make-bnf-location
       (- (bnf-mode-bnf-location-2 (bnf-mode-bnf-location-from-value bnf-paren)) 1)
       (bnf-mode-bnf-location-2 (bnf-mode-bnf-location-from-value bnf-paren))))))

   (t
    (bnf-mode-true))))
  
(defun bnf-mode-check-paren (bnf-paren v)
  (bnf-mode-boolean-and
   (bnf-mode-check-paren-exists bnf-paren v)
   (bnf-mode-check-paren-type bnf-paren v)))

(defun bnf-mode-check-quote (bnf-quote v)
  (if (bnf-mode-is-syntax-quote? v)
      (bnf-mode-true)
    (bnf-mode-false (bnf-mode-make-missing-quote v bnf-quote))))


(defun bnf-mode-check-constant (bnf-constant v)
  (if  (equal (bnf-mode-constant-1 bnf-constant)
	      (bnf-mode-constant-1 v))
      (bnf-mode-true)
    (bnf-mode-false
     (bnf-mode-make-error-constant
      v
      (bnf-mode-bnf-location-from-value bnf-constant)
      (bnf-mode-constant-1 bnf-constant)))))

;; Returns a elisp boolean
(defun bnf-mode-has-left-most-constnat? (v bnf-list)
  (and (bnf-mode-is-syntax-constant? (car v))
       (bnf-mode-is-constant? (car bnf-list))
       (equal (bnf-mode-syntax-constant-1 (car v))
	      (bnf-mode-constant-1 (car bnf-list)))))

;; bnf, bnf constructor
;; v, list of values
(defun bnf-mode-check-value-bnf (bnf v v-previous reference-bnf constant-context star-context)
  (cond

   ((bnf-mode-is-paren? bnf)
    (if (bnf-mode-is-paren-type-curl? (bnf-mode-paren-1 bnf))
	(bnf-mode-handle-paren
	 (bnf-mode-paren-2 bnf)
	 v
	 reference-bnf
	 star-context)
      (let ((check (bnf-mode-check-paren bnf (bnf-mode-ui-car v))))
	(cond

	 ;; 3.08.2013: So the parenthesis does not match, but if there
	 ;; is a constant inside the parenthesis, the error location
	 ;; in the BNF is enhanced.
	 ((bnf-mode-is-false? check)
	  (let ((inner-constant 
		 (bnf-mode-has-left-most-constnat?
		  (bnf-mode-paren-2 bnf)
		  (bnf-mode-syntax-paren-2 (bnf-mode-ui-car v)))))
	    ;(message "error in paren: %s\n%s\n%s\n%s\n\n" inner-res (bnf-mode-paren-2 bnf) (bnf-mode-syntax-paren-2 (bnf-mode-ui-car v)) v)
	  (bnf-mode-make-remainder
	   check
	   v
	   nil
	   (if inner-constant
	       inner-constant
	     constant-context)
	   nil)))

	 ;; If the BNF paren is empty, return
	 ((null (bnf-mode-paren-2 bnf))
	  (bnf-mode-make-remainder
	   check
	   (bnf-mode-syntax-paren-2 (bnf-mode-ui-car v))
	   nil
	   constant-context
	   nil))

	 (t
	  (let ((inner 
		 (bnf-mode-add-arity-to-remainder
		  (bnf-mode-add-paren-to-remainder
		   (bnf-mode-handle-paren
		    (bnf-mode-paren-2 bnf)
		    (bnf-mode-syntax-paren-2 (bnf-mode-ui-car v))
		    reference-bnf
		    star-context))
		  (bnf-mode-is-true? (bnf-mode-check-arity (bnf-mode-paren-2 bnf) (bnf-mode-syntax-paren-2 (bnf-mode-ui-car v)))))))
	    (cond
	     ;; If the inner was matched, continue on the rest
	     ((bnf-mode-is-remainder-match? inner)
	      (bnf-mode-make-remainder
	       (bnf-mode-true)
	       (bnf-mode-ui-cdr v)
	       nil
	       nil
	       nil))

	     ;; No all input was matched inside the paren
	     ((bnf-mode-is-remainder-true? inner)
	      (bnf-mode-update-remainder-with-new-error
	       inner
	       (bnf-mode-make-error-unmatched-input
		(bnf-mode-remainder-2 inner)
		(bnf-mode-bnf-location-from-value bnf))))
		


	     (t
	      (bnf-mode-add-context-value-if-missing inner v)))))))))
	       


   ((bnf-mode-is-constant? bnf)
    (let ((check-const (bnf-mode-check-constant bnf (bnf-mode-ui-car v))))
      (let ((const-rem (bnf-mode-make-remainder
			check-const
			(if (bnf-mode-is-true? check-const) 
			    (bnf-mode-ui-cdr v)
			  v)
			(bnf-mode-is-true? check-const) ;; arity match, if the const is bnf-mode-true
			(bnf-mode-is-true? check-const) ;; const match
			nil)))                 ;; paren match
	const-rem)))
     

   ;; ((bnf-mode-is-quote? bnf)
   ;;  (let ((bnf-mode-check-quote (bnf-mode-check-quote (bnf-mode-quote-1 bnf) (bnf-mode-ui-car v))))
   ;;    (if (bnf-mode-is-true? bnf-mode-check-quote)
   ;; 	  (bnf-mode-check-value-bnf 
   ;; 	   (bnf-mode-quote-1 bnf) 
   ;; 	   (bnf-mode-syntax-quote-1 (bnf-mode-ui-car v))
   ;; 	   v-previous 
   ;; 	   reference-bnf 
   ;; 	   constant-context 
   ;; 	   star-context)
   ;; 	(bnf-mode-make-remainder
   ;; 	 bnf-mode-check-quote
   ;; 	 v
   ;; 	 nil
   ;; 	 nil
   ;; 	 nil))))


   ((bnf-mode-is-group-ref? bnf)
    (let ((group-ref-result (bnf-mode-check-reference
			     bnf
			     (bnf-mode-ui-car v)
			     reference-bnf
			     constant-context
			     (bnf-mode-key-words-list-from-bnf-main reference-bnf))))
      (cond

       ((bnf-mode-is-remainder-match? group-ref-result)
	(bnf-mode-make-remainder
	 (bnf-mode-remainder-1 group-ref-result)
	 (bnf-mode-ui-cdr v)
	 (bnf-mode-remainder-3 group-ref-result)
	 (bnf-mode-remainder-4 group-ref-result)
	 (bnf-mode-remainder-5 group-ref-result)))
       
       (t
	group-ref-result))))

   ((bnf-mode-is-plus? bnf)
    (cond
     ((null v)
      (bnf-mode-make-remainder 
       (bnf-mode-false
	(bnf-mode-make-error-plus-at-least-one-match
	 v-previous
	 (bnf-mode-bnf-location-from-value bnf)))
       nil
       nil
       nil
       nil))

     (t
      (bnf-mode-handle-multiplier
       bnf
       (car (bnf-mode-plus-1 bnf))
       v
       reference-bnf
       constant-context))))


   ((bnf-mode-is-star? bnf)
    (cond
     ((null v)
      (bnf-mode-make-remainder (bnf-mode-true) v t nil nil))

     (t
      (bnf-mode-handle-multiplier
       bnf
       (car (bnf-mode-star-1 bnf))
       v
       reference-bnf
       constant-context))
	))

       
       
   (t
    (error "No match: member-values=%s\nv=%s" bnf v))))
  
;;;



(defun bnf-mode-handle-multiplier (bnf inner-bnf v reference-bnf constant-context)
  (let ((iter (bnf-mode-check-value-bnf
	       inner-bnf
	       v
	       v
	       reference-bnf
	       constant-context
	       t)))

    (cond
     ((bnf-mode-is-remainder-match? iter)
      iter)

     ((bnf-mode-is-remainder-false? iter)
      iter)

     ((bnf-mode-is-remainder-true? iter)
      (bnf-mode-make-updated-remainder
       (bnf-mode-check-value-bnf
	bnf
	(bnf-mode-remainder-2 iter)
	(bnf-mode-remainder-2 iter)
	reference-bnf
	constant-context
	t)
       iter))

     (t
      (error "handle-multiplier, unhandled case: %s" iter)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bnf-mode-is-bnf-match? (member-values v)
  (and (null member-values)
       (null v)))

(defun bnf-mode-is-bnf-non-match? (member-values v)
  (and (null member-values)
       (not (null v))))

;;;;;;;;;;;;;;;;;;;;;;; Look up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bnf-mode-look-up-reference (bnf ref-name)
  (let ((res (bnf-mode-fold-left
	      (bnf-mode-groups-1 (bnf-mode-bnf-1 bnf))	      
	      (lambda (bnf-mode-x rest)
		(cond
		 (rest
		  rest)
		 
		 ((equal (bnf-mode-group-1 bnf-mode-x) ref-name)
		  (bnf-mode-group-2 bnf-mode-x))
		 
		 (t 
		  nil)))
	      (lambda () nil))))

    (if res
	(bnf-mode-make-reference-found res)
      (let ((buildin (bnf-mode-is-build-in-production? ref-name)))
	(cond
	 ((bnf-mode-is-normal-buildin? buildin)
	  (bnf-mode-make-reference-buildin buildin))

	 ((bnf-mode-is-keyword-list-buildin? buildin)
	  (bnf-mode-make-reference-buildin buildin))

	 ((bnf-mode-is-non-buildin? buildin)
	  (bnf-mode-make-reference-not-found ref-name)))))))

;; Spelled built-in!
(defun bnf-mode-handle-buildin-reference (result v_ group-ref)
  (if (bnf-mode-is-false? result)
      (cond

       ((bnf-mode-is-reference-build-in-nothing? group-ref)
	(bnf-mode-make-remainder
	 (bnf-mode-true)
	 v_
	 nil
	 nil
	 nil))

       (t
	(bnf-mode-make-remainder
	 (bnf-mode-false
	  (bnf-mode-make-error-group-ref-expected-buildin 
	   v_
	   (bnf-mode-bnf-location-from-value group-ref)
	   (bnf-mode-false-1 result)))
	 v_
	 nil
	 nil
	 nil)))
    (bnf-mode-make-remainder
     result
     nil
     nil
     nil
     nil)))


(defun bnf-mode-check-reference (group-ref v_ reference-bnf constant-context key-words-list)
  "group-ref sexp reference-bnf"
  (let ((lookup (bnf-mode-look-up-reference 
		 reference-bnf 
		 (bnf-mode-group-ref-1 group-ref))))
 
    (cond 

     ((bnf-mode-is-reference-found? lookup)
      (bnf-mode-handle-members-lookup
       group-ref
       (bnf-mode-members-1 (bnf-mode-reference-found-1 lookup))
       v_
       reference-bnf
       constant-context))

     ((bnf-mode-is-reference-buildin? lookup)
      (cond
       
       ((bnf-mode-is-normal-buildin? (bnf-mode-reference-buildin-1 lookup))
	(bnf-mode-handle-buildin-reference 
	 (funcall 
	  (bnf-mode-normal-buildin-2 (bnf-mode-reference-buildin-1 lookup))
	  v_)
	 v_ 
	 group-ref))

       ((bnf-mode-is-keyword-list-buildin? (bnf-mode-reference-buildin-1 lookup))
	(bnf-mode-handle-buildin-reference 
	 (funcall 
	  (bnf-mode-keyword-list-buildin-2 (bnf-mode-reference-buildin-1 lookup))
	  v_
	  key-words-list)
	 v_ 
	 group-ref))))
			    

     ((bnf-mode-is-reference-not-found? lookup)
      (bnf-mode-make-remainder
       (bnf-mode-false
	(bnf-mode-make-error-group-ref-not-found
	 v_
	 (bnf-mode-bnf-location-from-value group-ref)
	 (bnf-mode-group-ref-1 group-ref)))
       v_
       nil
       nil
       nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun bnf-mode-handle-paren (bnf v reference-bnf star-context)
  (if (bnf-mode-has-member-multiplier? bnf)
      (bnf-mode-handle-member-with-multiplier bnf v reference-bnf)
    (bnf-mode-handle-member-without-multiplier bnf v reference-bnf star-context nil)))

(defun bnf-mode-relevant-value (v1 v2)
  (if (null v1)
      v2
    v1))

;; Checks the values,v, against the bnf-list, iteratively left to right
(defun bnf-mode-check-value-iter (bnf-list v v-previous reference-bnf previous-bnf arity-match constant-context)
  (cond

   ;;; Terminate with error, if the input is null but there are still members left
   ;;; to be matched
   ((bnf-mode-is-bnf-non-match? bnf-list v)
    (bnf-mode-make-remainder
     (bnf-mode-false
      (bnf-mode-make-error-unmatched-input v (bnf-mode-bnf-location-from-value previous-bnf)))
     v
     nil
     nil
     nil))

   ;; Terminate with true, if the input is empty and there are no
   ;; members to match against.
   ((bnf-mode-is-bnf-match? bnf-list v)
    (bnf-mode-make-remainder 
     (bnf-mode-true)
     nil
     nil
     nil
     nil))

   (t
    (let ((iter-res 
	   (bnf-mode-fold-left
	    bnf-list
	    (lambda (bnf rest)
	      (cond
	
	       ((and (bnf-mode-is-remainder-false? rest)
		     (not (bnf-mode-is-worst-possible-error? (bnf-mode-false-1 (bnf-mode-remainder-1 rest)))))
		rest)

	       (t
		(let* ((new-match 
		       (bnf-mode-check-value-bnf
			bnf
			(bnf-mode-remainder-2 rest)
			v-previous
			reference-bnf
			(bnf-mode-remainder-4 rest)
			nil))
		       (better-err
			(bnf-mode-ensure-error-has-value-context
			 (bnf-mode-ensure-progress
			  (bnf-mode-make-updated-remainder new-match rest) 
			  rest
			  bnf)
			 v
			 bnf
			 (bnf-mode-remainder-4 rest)
			 (bnf-mode-remainder-4 new-match))))

		  (cond
		   ((and (bnf-mode-is-group-ref? bnf)
			 (bnf-mode-is-remainder-false? new-match))

		    (bnf-mode-make-updated-remainder
		     (bnf-mode-ensure-error-has-value-context
		      new-match
		      v
		      bnf
		      (bnf-mode-remainder-4 rest)
		      (bnf-mode-remainder-4 new-match))
		     rest))

		   ((bnf-mode-is-remainder-match? new-match)
		    (bnf-mode-make-updated-remainder new-match rest))

		   (t
		    better-err))))))
	   
	    (lambda () 
	      (bnf-mode-make-remainder
	       (bnf-mode-false (bnf-mode-make-worst-possible-error))
	       v
	       nil
	       constant-context
	       nil)))))

      iter-res))))


;; Adds value and bnf location to an error iff it is missing a value
;; context.
(defun bnf-mode-ensure-error-has-value-context 
  (r v bnf constant-context-current constant-context-inner)
  (if (not (null (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))
      r
    (cond
     ((bnf-mode-is-remainder-true? r)
      r)

     ((and (bnf-mode-is-group-ref? bnf)
	   (bnf-mode-is-remainder-false? r)
	   (bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	   constant-context-current
	   constant-context-inner)
      (bnf-mode-make-remainder
       (bnf-mode-false
	(bnf-mode-make-error-no-match-in-reference
	 (if (bnf-mode-inner-most-value (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
	     (bnf-mode-inner-most-value (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
	   (bnf-mode-inner-most-value v))
	 (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	 (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))
       (bnf-mode-remainder-2 r)
       (bnf-mode-remainder-3 r)
       (bnf-mode-remainder-4 r)
       (bnf-mode-remainder-5 r)))

     ((and (bnf-mode-is-group-ref? bnf)
	   (bnf-mode-is-remainder-false? r)
	   (bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	   constant-context-inner)
      (bnf-mode-make-remainder
       (bnf-mode-false
	(bnf-mode-make-error-no-match-in-reference
	 (if (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	     (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	   (bnf-mode-inner-most-value v))
	   
	 (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	 (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))
       (bnf-mode-remainder-2 r)
       (bnf-mode-remainder-3 r)
       (bnf-mode-remainder-4 r)
       (bnf-mode-remainder-5 r)))



     ((and (bnf-mode-is-group-ref? bnf)
	   (bnf-mode-is-remainder-false? r)
	   (bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	   constant-context-current)
      (let ((r (bnf-mode-make-remainder
		(bnf-mode-false
		 (bnf-mode-make-error-no-match-in-reference
		  (if (bnf-mode-remainder-2 r)
		      (bnf-mode-remainder-2 r)
		    (bnf-mode-inner-most-value v))
		  (bnf-mode-bnf-location-from-value bnf)
		  (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))
		(bnf-mode-remainder-2 r)
		(bnf-mode-remainder-3 r)
		(bnf-mode-remainder-4 r)
		(bnf-mode-remainder-5 r))))
	r))


     ((and (bnf-mode-is-group-ref? bnf)
	   (bnf-mode-is-remainder-false? r)
	   (bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
      (bnf-mode-make-remainder
       (bnf-mode-false
	(bnf-mode-make-error-no-match-in-reference
	 (if (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	     (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	   (bnf-mode-inner-most-value v))
	 (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	 (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))
       (bnf-mode-remainder-2 r)
       (bnf-mode-remainder-3 r)
       (bnf-mode-remainder-4 r)
       (bnf-mode-remainder-5 r)))

     (t
      r))))
     
 

 

(defun bnf-mode-ensure-progress (a b bnf)
  (cond

   ;; If the new error is false, and the previous was true,
   ;; obviously report the new error
   ((and (bnf-mode-is-remainder-false? a)
	 (bnf-mode-is-remainder-true? b))
    a)

   ((bnf-mode-is-remainder-match? a)
    a)

   ((bnf-mode-is-remainder-match? b)
    b)

   
   ((bnf-mode-is-remainder-true? a)
    a)

   ((and (bnf-mode-is-remainder-true? a)
	 (bnf-mode-is-remainder-true? b))
    (bnf-mode-select-shortest-remainder a b))

   (t
    (bnf-mode-select-better-error a b ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Error selection  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remainder context predicates
(defun bnf-mode-has-none? (r)
  (and (not (bnf-mode-has-paren? r))
       (not (bnf-mode-has-constant? r))
       (not (bnf-mode-has-arity? r))))

(defun bnf-mode-has-paren? (r)
  (bnf-mode-remainder-5 r))

(defun bnf-mode-has-constant? (r)
  (bnf-mode-remainder-4 r))

(defun bnf-mode-has-arity? (r)
  (bnf-mode-remainder-3 r))

(defun bnf-mode-has-arity-and-constant? (r)
  (and (bnf-mode-has-arity? r)
       (bnf-mode-has-constant? r)))

(defun bnf-mode-has-arity-and-paren? (r)
  (and (bnf-mode-has-arity? r)
       (bnf-mode-has-paren? r)))


(defun bnf-mode-has-paren-and-constant? (r)
  (and (bnf-mode-has-paren? r)
       (bnf-mode-has-constant? r)))

(defun bnf-mode-has-arity-and-paren-and-constant? (r)
  (and (bnf-mode-has-arity-and-paren? r)
       (bnf-mode-has-constant? r)))

;;;


;; Used to index in the error table
(defun bnf-mode-remainder-state-index (r)
  (cond
   ((bnf-mode-has-none? r) 7)
   ((bnf-mode-has-arity-and-paren-and-constant? r) 6)
   ((bnf-mode-has-arity-and-paren? r) 5)
   ((bnf-mode-has-arity-and-constant? r) 4)
   ((bnf-mode-has-paren-and-constant? r) 3)
   ((bnf-mode-has-arity? r) 2)
   ((bnf-mode-has-paren? r) 1)
   ((bnf-mode-has-constant? r)  0)))


;; Given two errors, decides which error is picked over an other
;; error.
(defun bnf-mode-make-error-cmp-table ()
  (list ;  c    p    a   cp   ca   pa   cpa  none |a / b
   (list 'len 'er2 'er2 'er1 'er1 'er2 'er1 'er2) ; c
   (list 'er1 'len 'len 'er1 'er1 'er1 'er1 'er2) ; p
   (list 'er1 'len 'len 'er1 'er1 'er1 'er1 'er2) ; a
   (list 'er2 'er2 'er2 'len 'er2 'er2 'er1 'er2) ; cp
   (list 'er2 'er2 'er2 'er1 'len 'er2 'er1 'er2) ; ca
   (list 'er1 'er2 'er2 'er1 'er1 'len 'er1 'er2) ; pa
   (list 'er2 'er2 'er2 'er2 'er2 'er2 'len 'er2) ; cpa
   (list 'er1 'er1 'er1 'er1 'er1 'er1 'er1 'len) ; none
  ))


;; todo ugly as hell, should be cleaned up
(defun bnf-mode-select-better-error (a b)
  (cond 

   ((and (bnf-mode-is-remainder-false? a)
	 (bnf-mode-is-worst-possible-error? (bnf-mode-false-1 (bnf-mode-remainder-1 a))))
    b)

   ((and (bnf-mode-is-remainder-false? b)
	 (bnf-mode-is-worst-possible-error? (bnf-mode-false-1 (bnf-mode-remainder-1 b))))
    a)

   (t

    

  ;; 'er1 is a
  ;; 'len is a.length <= b.length, return a, else b
  ;; 'er2 is b
    (let* ((tbl (bnf-mode-make-error-cmp-table))
	   (res (nth (bnf-mode-remainder-state-index a) 
		     (nth (bnf-mode-remainder-state-index b) tbl))))
      (cond
      

       ((bnf-mode-is-remainder-match? a)
	a)

       ((bnf-mode-is-remainder-match? b)
	b)

       ((equal res 'er1) a)
       ((equal res 'er2) b)
       ((equal res 'len)
	(bnf-mode-select-shortest-remainder a b))

       (t
	(error "select-better-error unahendled case: %s %s" a b))

       )))))


(defun bnf-mode-select-shortest-remainder (a b)
  (if (<= (bnf-mode-nested-length-of-sexp-value (bnf-mode-remainder-2 a))
	  (bnf-mode-nested-length-of-sexp-value (bnf-mode-remainder-2 b)))
      a
    b))



;;; Remainder modifiers

(defun bnf-mode-make-updated-remainder (new old)
  (bnf-mode-make-remainder
   (bnf-mode-remainder-1 new)
   (bnf-mode-remainder-2 new)
   (or (bnf-mode-remainder-3 old)
       (bnf-mode-remainder-3 new))
   (or (bnf-mode-remainder-4 old)
       (bnf-mode-remainder-4 new))
   (or (bnf-mode-remainder-5 old)
       (bnf-mode-remainder-5 new))))


(defun bnf-mode-add-paren-to-remainder (r)
  (bnf-mode-make-remainder
   (bnf-mode-remainder-1 r)
   (bnf-mode-remainder-2 r)
   (bnf-mode-remainder-3 r)
   (bnf-mode-remainder-4 r)
   t))

(defun bnf-mode-add-constant-to-remainder (r constant-context)
  (bnf-mode-make-remainder
   (bnf-mode-remainder-1 r)
   (bnf-mode-remainder-2 r)
   (bnf-mode-remainder-3 r)
   constant-context
   (bnf-mode-remainder-5 r)))




(defun bnf-mode-add-arity-to-remainder (r arity)
  (bnf-mode-make-remainder
   (bnf-mode-remainder-1 r)
   (bnf-mode-remainder-2 r)
   arity
   (bnf-mode-remainder-4 r)
   (bnf-mode-remainder-5 r)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bnf-mode-is-multiplier? (v)
  (or (bnf-mode-is-star? v)
      (bnf-mode-is-plus? v)))

;; Takes a member list as input: (member ((constant a) (constant b)))
(defun bnf-mode-has-member-multiplier? (v)
  (bnf-mode-fold-left-break-at
   v
   (lambda (bnf-mode-x rest) 
     (bnf-mode-is-multiplier? bnf-mode-x))
   (lambda () nil)
   (lambda (bnf-mode-x rest) 
     (bnf-mode-is-multiplier? bnf-mode-x))))

;;;;;;;;;;; helpers for remainder inner values ;;;;;;;;;;;;;;

(defun bnf-mode-is-remainder-true? (v)
  (bnf-mode-is-true? (bnf-mode-remainder-1 v)))

(defun bnf-mode-is-remainder-false? (v)
  (bnf-mode-is-false? (bnf-mode-remainder-1 v)))

(defun bnf-mode-is-remainder-match? (v)
  (and (bnf-mode-is-true? (bnf-mode-remainder-1 v))
       (null (bnf-mode-remainder-2 v))))

(defun bnf-mode-is-remainder-partly-match? (v)
  (and (bnf-mode-is-true? (bnf-mode-remainder-1 v))
       (not (null (bnf-mode-remainder-2 v)))))



(defun bnf-mode-is-input-less-than-expected? (arity)
  "arity: boolean --> elisp-boolean"
  (>
   (bnf-mode-error-arity-1 (bnf-mode-false-1 arity))
   (bnf-mode-error-arity-2 (bnf-mode-false-1 arity))))

   
   


;; m, is the member.
;; v, is the input value.
;; reference-bnf, is the full bnf AST.
(defun bnf-mode-handle-member-without-multiplier (member-list v reference-bnf star-context constant-context)
  
  (let* ((arity ;;(bnf-mode-true))

	  (if star-context 
	  	    (bnf-mode-true)
	  	  (bnf-mode-check-arity member-list v)))

	 (rest
	   (bnf-mode-check-value-iter
	    member-list
	    v
	    v
	    reference-bnf
	    member-list
	    (bnf-mode-is-true? arity)
	    constant-context)))


    (cond
     ((bnf-mode-is-true? arity)
      (cond
       ((bnf-mode-is-remainder-match? rest)
	rest)
       
       (t
	rest)


       ))

     ((bnf-mode-is-false? arity)
      (cond
       ((not (bnf-mode-is-remainder-match? rest))
	rest)

       ((bnf-mode-is-false? (bnf-mode-remainder-1 rest))
	(bnf-mode-make-remainder
	 (bnf-mode-false
	  (bnf-mode-make-error-arity-nested 
	   v 
	   (bnf-mode-bnf-location-from-value (car member-list))
	   (bnf-mode-false-1 arity)
	   (bnf-mode-false-1 (bnf-mode-remainder-1 rest))))
	 (bnf-mode-remainder-2 rest)
	 (bnf-mode-remainder-3 rest)
	 (bnf-mode-remainder-4 rest)
	 (bnf-mode-remainder-5 rest)))

       ((bnf-mode-is-true? (bnf-mode-remainder-1 rest))
	(cond
	 ;; Too less input (i.e. some inner bnf construction could be of kleene star type)
	 ;;
	 ;; <a> ::= (a b c)
	 ;; inp: (a b)	 
	 ((bnf-mode-is-input-less-than-expected? arity)
	  (cond
	   ;; The arity check is allowed to fail, since in an expression
	   ;; like (a <b> <b>) the <b> references might point to
	   ;; something of type kleene star. If the rest part is a
	   ;; complete match, all is good.
	   ((bnf-mode-is-remainder-match? rest)
	    rest)

	   (t
	    (bnf-mode-make-remainder 
	     arity
	     (bnf-mode-remainder-2 rest)
	     nil
	     (bnf-mode-remainder-4 rest)
	     (bnf-mode-remainder-5 rest)))))

	 ;; Too less input (i.e. some inner bnf construction could be of kleene star type)
	 ;;
	 ;; <a> ::= (a b)
	 ;; inp: (a b c)	 
	 ;; ((is-input-more-than-expected? arity)
	 ;;  (bnf-mode-make-remainder 
	 ;;   arity
	 ;;   (bnf-mode-remainder-2 rest)
	 ;;   nil
	 ;;   (bnf-mode-remainder-4 rest)
	 ;;   (bnf-mode-remainder-5 rest)))


	 (t
	  rest)))

       (t
	(bnf-mode-make-remainder
	 arity
	 v
	 nil
	 (bnf-mode-remainder-4 rest)
	 (bnf-mode-remainder-5 rest)))



	))

     (t
      (error "handle-member-without-multiplier, unhandled case: %s" arity)))))


(defun bnf-mode-first-non-multiplier (v)
  (bnf-mode-fold-left-break-before
   v
   (lambda (bnf-mode-x rest)
     (if (bnf-mode-is-multiplier? bnf-mode-x)
	 rest
       (append 
	rest
	(list bnf-mode-x))))
   (lambda () '())
   (lambda (bnf-mode-x rest)
     (bnf-mode-is-multiplier? bnf-mode-x))))


(defun bnf-mode-last-non-multiplier (v)
  (bnf-mode-fold-right-break-before
   v
   (lambda (bnf-mode-x rest)
     (if (bnf-mode-is-multiplier? bnf-mode-x)
	 rest
       (cons bnf-mode-x rest)))
   (lambda () '())
   (lambda (bnf-mode-x rest)
     (bnf-mode-is-multiplier? bnf-mode-x))))

;; At this point the following type of expressions are not supported:
;; (a* a a*)
;;
;; However the following is supported:
;; (abc {<ref>}*)
;; (abc {<ref>}* def)
;; (a*)
(defun bnf-mode-handle-member-with-multiplier (member-list v reference-bnf)
  (let* ((first-n (bnf-mode-first-non-multiplier member-list))
	(last-n (bnf-mode-last-non-multiplier member-list))
	(middle-n (bnf-mode-remove-first-n-last-m 
		   (length first-n)
		   (length last-n)
		   member-list)))
    (let ((left-res (if (< 0 (length first-n))
			 (bnf-mode-handle-member-without-multiplier 
			  first-n
			  (bnf-mode-n-first-of-list (length first-n) v)
			  reference-bnf
			  nil
			  nil)
		       (bnf-mode-make-remainder 
			(bnf-mode-true)
			nil
			nil
			nil
			nil))))
      (if (bnf-mode-is-remainder-false? left-res)
	  left-res
	(let*

	    ;; The (a*) case or a+
	    ((middle-res (bnf-mode-check-value-iter
			  middle-n
			  (bnf-mode-remove-first-n-last-m
			   (length first-n)
			   (length last-n)
			   v)
			  v
			  reference-bnf
			  member-list
			  t
			  (bnf-mode-remainder-4 left-res)))

	     (right-res (if (< 0 (length last-n))
			    (bnf-mode-handle-member-without-multiplier 
			     last-n
			     (bnf-mode-n-last-of-list (length last-n) v)
			     reference-bnf
			     nil
			     (bnf-mode-remainder-4 middle-res))
			  (bnf-mode-make-remainder
			   (bnf-mode-true)
			   nil
			   nil
			   nil
			   nil))))


	  (cond
	   ((bnf-mode-is-remainder-false? left-res)
	    left-res)

	   ((bnf-mode-is-remainder-false? middle-res)
	    middle-res)

	   ((bnf-mode-is-remainder-false? right-res)
	    right-res)

	   (t
	    (bnf-mode-make-remainder 
	     (bnf-mode-true) 
	     nil 
	     (or (bnf-mode-remainder-3 left-res)
		 (bnf-mode-remainder-3 middle-res)
		 (bnf-mode-remainder-3 right-res))
	     (or (bnf-mode-remainder-4 left-res)
		 (bnf-mode-remainder-4 middle-res)
		 (bnf-mode-remainder-4 right-res))
	     (or (bnf-mode-remainder-5 left-res)
		 (bnf-mode-remainder-5 middle-res)
		 (bnf-mode-remainder-5 right-res))))))))))


;; Converts the final remainder to bnf-mode-true or an error.
(defun bnf-mode-check-final-match (match bnf)
  (cond
   ((bnf-mode-is-remainder-match? match)
    (bnf-mode-true))
   
   ((bnf-mode-is-remainder-false? match)
    (bnf-mode-remainder-1 match))

   ((bnf-mode-is-remainder-partly-match? match)
    (bnf-mode-false
     (bnf-mode-make-error-unmatched-input 
      (bnf-mode-remainder-2 match)
      (bnf-mode-extended-bnf-location-from-value bnf))))

   ((bnf-mode-is-remainder-group-ref? match)
    (bnf-mode-remainder-group-ref-1 match))

   
   (t
    (error "handle-member-with-multiplier: %s" match))))





(defun bnf-mode-add-context-value-to-error-if-needed (r v bnf-loc constant-context)
  (cond
   ((bnf-mode-is-remainder-false? r)
    ;; bnf-mode-remainder-4 is constant context of the inner
    (if (and constant-context
	     (not (bnf-mode-remainder-4 r)))
	(bnf-mode-update-remainder-with-new-error
	 r
	 (bnf-mode-add-bnf-location-and-context-value-to-error
	  (bnf-mode-remainder-1 r)
	  bnf-loc
	  (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))
      r))

   ((bnf-mode-is-remainder-true? r)
      r)

   (t
    (bnf-mode-make-remainder
     (if (null (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
	 (bnf-mode-add-context-value-to-error (bnf-mode-remainder-1 r) v)
       (bnf-mode-remainder-1 r))
      (bnf-mode-remainder-2 r)
      (bnf-mode-remainder-3 r)
      (bnf-mode-remainder-4 r)
      (bnf-mode-remainder-5 r)))))


(defun bnf-mode-inner-most-value (v)
  v)
  ;; (cond
  ;;  ((bnf-mode-is-constant? v)
  ;;   v)

  ;;  ((bnf-mode-is-syntax-paren? v)
  ;;   (bnf-mode-last-element-of-list
  ;;    (bnf-mode-syntax-paren-2 v)))

  ;;  (t
  ;;   (bnf-mode-last-element-of-list v))))

(defun bnf-mode-update-remainder-with-inner-error (r v v-remainder bnf-location)
  "Returns a remainder with a the inner error bnf-mode-update if missing value context: remainder value bnf-location"
  (let* ((inner-error 
	 (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
	 (new-err-value
	  (if (null v-remainder)
	      (bnf-mode-inner-most-value v)
	    v-remainder)))
    (bnf-mode-make-remainder
     (bnf-mode-false
      (bnf-mode-update-error 
       (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
       new-err-value
       bnf-location))
     (bnf-mode-remainder-2 r)
     (bnf-mode-remainder-3 r)
     (bnf-mode-remainder-4 r)
     (bnf-mode-remainder-5 r))))


(defun bnf-mode-update-remainder-with-missing-syntax-error (r v bnf-location)
  "Returns a remainder with a missing syntax error: remainder value bnf-location"
  (bnf-mode-make-remainder
   (bnf-mode-false
    (bnf-mode-make-error-missing-syntax
     v
     bnf-location))
   (bnf-mode-remainder-2 r)
   (bnf-mode-remainder-3 r)
   (bnf-mode-remainder-4 r)
   (bnf-mode-remainder-5 r)))


(defun bnf-mode-add-bnf-location-and-context-value-to-remainder-and-error (r bnf-loc v)
  (bnf-mode-make-remainder
   (bnf-mode-false
    (bnf-mode-add-bnf-location-and-context-value-to-error (bnf-mode-remainder-1 r) bnf-loc v))
   (bnf-mode-extract-nested-value-if-necessary r v)
   (bnf-mode-remainder-3 r)
   (bnf-mode-remainder-4 r)
   (bnf-mode-remainder-5 r)))


(defun bnf-mode-extract-nested-value-if-necessary (r v)
  (cond 
   ((bnf-mode-is-remainder-false? r)
    (if (null v)
	(cond
	 
	 ((bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	  (bnf-mode-value-from-error (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))

	 (t
	  (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))
      v))

   (t
    v)))


(defun bnf-mode-extract-nested-bnf-if-necessary (r bnf-reference)
  (cond
   ((bnf-mode-has-constant? r)
    (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r))))

   (t
    (bnf-mode-bnf-location-from-value bnf-reference))))
    
	  
;; boolean * bnf-location * sexp-value --> error
(defun bnf-mode-add-bnf-location-and-context-value-to-error (e bnf-loc v)
  (cond

   ((bnf-mode-is-true? e)
    e)

   ((bnf-mode-is-false? e)

    (cond

     ((bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 e))
      (bnf-mode-false-1 e))
   

     ((bnf-mode-is-error-arity? (bnf-mode-false-1 e))
      (bnf-mode-make-error-arity
       v
       bnf-loc
       (bnf-mode-error-arity-1 (bnf-mode-false-1 e))
       (bnf-mode-error-arity-2 (bnf-mode-false-1 e))))
     
     (t
      (bnf-mode-update-error (bnf-mode-false-1 e) v bnf-loc))))

   (t
    (error "Non boolean given: %s" e))))



(defun bnf-mode-add-context-value-to-error (e v)
  (cond

   ((bnf-mode-is-true? e)
    e)

   (t
    (bnf-mode-update-error-value e v))
     
))     


(defun bnf-mode-has-unmatched? (r)
  (< 0 (bnf-mode-nested-length-of-sexp-value 
	(bnf-mode-remainder-2 r))))



(defun bnf-mode-is-remainder-unmatched-input? (r)
  (and (bnf-mode-is-remainder-false? r)
       (bnf-mode-is-error-unmatched-input? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))))



;; if there is no remainder in r, append some "context value" to the
;; result for improved syntactic underlining.
(defun bnf-mode-maybe-append-context-value-and-bnf-loc (r bnf-loc v)
  (cond

   ;; If the remainder's error is unmatched input, report the error at
   ;; "this" location
   ((bnf-mode-is-remainder-unmatched-input? r)
     (bnf-mode-add-bnf-location-and-context-value-to-remainder-and-error
     r
     bnf-loc
     v))

   ((bnf-mode-has-unmatched? r)
    r)

   (t
    (bnf-mode-add-bnf-location-and-context-value-to-remainder-and-error
     r
     bnf-loc
     v))))




(defun bnf-mode-wrap-in-error-no-match-in-reference (r v-err bnf-err)
  "remainder value-error bnf-error"
  (bnf-mode-make-remainder
   (bnf-mode-false
    (bnf-mode-make-error-no-match-in-reference
     v-err
     bnf-err
     (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
   (bnf-mode-remainder-2 r)
   (bnf-mode-remainder-3 r)
   (bnf-mode-remainder-4 r)
   (bnf-mode-remainder-5 r)))



;; bnf-location, location of the production under lookup	
(defun bnf-mode-handle-members-lookup (bnf-reference member-list v reference-bnf constant-context)
  (let ((member-match-result 
	 (bnf-mode-fold-left
	  member-list
	  (lambda (bnf-mode-x rest) 

	    (cond
	     ((bnf-mode-has-constant? rest)
	      rest)

	     ((bnf-mode-is-remainder-match? rest)
	      rest)

	     (t
	      (let ((new-member-match
		     (bnf-mode-check-bnf
		      bnf-mode-x
		      v 
		      reference-bnf)))

		(bnf-mode-ensure-progress
		 new-member-match 
		 rest 
		 bnf-mode-x)))))
	  

	  

	  (lambda () 
	    (bnf-mode-make-remainder
	     (bnf-mode-false (bnf-mode-make-worst-possible-error))
	     v
	     nil
	     nil
	     nil)))))

    ;; if the error was in side a construct with a paren and a const,
    ;; point to that construct

;;    (message "paren-type-mismatch: %s\n%s\n%s\n\n" bnf-reference v member-match-result)
    (cond 

     ;; If the the reference only has one member and the match failed
     ;; at that member, report the error at that location.
     ;; Added as of 30.08.2013
     ((and (bnf-mode-is-remainder-false? member-match-result)
     	   (equal (length member-list) 1))
      member-match-result)

     ;; Parenthesis are a bit special. It is ok to point out a
     ;; parenthesis error, before following other references.
     ((and (bnf-mode-is-remainder-false? member-match-result)
	   (bnf-mode-is-error-paren-type-mismatch? (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	   (bnf-mode-remainder-4 member-match-result))  ;; Check that the constant matches
;      (message "paren-type-mismatch: %s" member-match-result)
      member-match-result)

     ;; Missing syntax is a special case, indicating that some context
     ;; value and bnf location has to be injected
     ((and (bnf-mode-is-remainder-false? member-match-result)
	   (bnf-mode-is-error-missing-syntax? (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result))))
      (bnf-mode-update-remainder-with-missing-syntax-error 
       member-match-result
       v
       (bnf-mode-bnf-location-from-value bnf-reference)))


     ((and (bnf-mode-is-remainder-false? member-match-result)
	   (bnf-mode-is-error-plus-at-least-one-match? (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result))))
      (bnf-mode-make-remainder
       (bnf-mode-false
	(bnf-mode-make-error-no-match-in-reference
	 (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	 (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	 (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result))))
       (bnf-mode-remainder-2 member-match-result)
       (bnf-mode-remainder-3 member-match-result)
       constant-context
       (bnf-mode-remainder-5 member-match-result)))

     ((and (bnf-mode-is-remainder-false? member-match-result)
     	   (bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
     	   constant-context
     	   (bnf-mode-remainder-4 member-match-result))
      member-match-result)



     ;; No match in reference is a special case, indicating that an
     ;; error should be reported at a lower level if the needed
     ;; location information is present.
     ((and (bnf-mode-is-remainder-false? member-match-result)
	   (bnf-mode-is-error-no-match-in-reference? (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	   constant-context)
      (cond 

       ;; If no constant was matced in the inner paren, but a constant
       ;; missing error, report the inner most error.
       ;; ((bnf-mode-is-error-constant? (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result))))
       ;; 	member-match-result)
       

       ;; The generic case: If no constant was matched in the inner
       ;; paren, report the error at the outer level.
       (t
	(bnf-mode-make-remainder
	 (bnf-mode-false
	  (bnf-mode-make-error-no-match-in-reference
	   (bnf-mode-inner-most-value v)
;	   (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	   (bnf-mode-bnf-location-from-value bnf-reference)
;	   (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	   (bnf-mode-error-no-match-in-reference-1 (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))))
	 (bnf-mode-remainder-2 member-match-result)
	 (bnf-mode-remainder-3 member-match-result)
	 constant-context
	 (bnf-mode-remainder-5 member-match-result)))))



     ((bnf-mode-is-remainder-false? member-match-result)
      (cond
     
       ((and constant-context
	     (bnf-mode-remainder-4 member-match-result))
	(bnf-mode-wrap-in-error-no-match-in-reference
	 member-match-result
	 (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	 (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))))
       
       ((and (not constant-context)
	     (bnf-mode-remainder-4 member-match-result))
	(bnf-mode-wrap-in-error-no-match-in-reference
	 member-match-result
	 (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	 (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))))


       
       ((and constant-context
	     (not (bnf-mode-remainder-4 member-match-result)))
	(bnf-mode-wrap-in-error-no-match-in-reference
	 member-match-result
	 v
	 (bnf-mode-bnf-location-from-value bnf-reference)))


       ;; Point at the upper level bnf, but at the lower level syntax
       ((and (not constant-context)
	     (not (bnf-mode-remainder-4 member-match-result)))
	(bnf-mode-wrap-in-error-no-match-in-reference
	 member-match-result
	 (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	 (bnf-mode-bnf-location-from-value bnf-reference))
	

	;; (bnf-mode-wrap-in-error-no-match-in-reference
	;;  member-match-result
	;;  (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result)))
	;;  (bnf-mode-bnf-location-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 member-match-result))))

	)))

	   
     ((bnf-mode-is-remainder-match? member-match-result)
      (bnf-mode-add-context-value-if-missing 
       member-match-result
       v))

     ((bnf-mode-has-arity-and-paren-and-constant? member-match-result)
      (bnf-mode-maybe-append-context-value-and-bnf-loc
       member-match-result
       (bnf-mode-bnf-location-from-value bnf-reference)
       v))

          
     ((bnf-mode-has-arity-and-constant? member-match-result)
      (bnf-mode-maybe-append-context-value-and-bnf-loc
       member-match-result
       (bnf-mode-bnf-location-from-value bnf-reference)
       v))

     ((bnf-mode-has-paren-and-constant? member-match-result)
      (bnf-mode-add-context-value-if-missing 
       (bnf-mode-handle-possible-error
	member-match-result
	bnf-reference
	constant-context)
       v))
      
     ((bnf-mode-has-constant? member-match-result)
      (bnf-mode-add-context-value-if-missing 
       (bnf-mode-handle-possible-error
	member-match-result
	bnf-reference
	constant-context)
       v))
      
     (t
      member-match-result))))

(defun bnf-mode-add-context-value-if-missing (r v)
  "Adds some syntax context to the remainder: remainder context-value"
  (cond
   ((bnf-mode-is-remainder-true? r)
    r)

   ((bnf-mode-is-remainder-false? r)
    (cond
     ((null (bnf-mode-value-from-error (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
      (bnf-mode-update-remainder-with-new-error
       r
       (bnf-mode-update-error-value 
	(bnf-mode-false-1 (bnf-mode-remainder-1 r))
	v)))

     (t
      r)))))
       

;; Use this to bnf-mode-update remainders with new errors
(defun bnf-mode-update-remainder-with-new-error (r e-new)
  "remainder new-error"
  (bnf-mode-make-remainder
   (bnf-mode-false e-new)
   (bnf-mode-remainder-2 r)
   (bnf-mode-remainder-3 r)
   (bnf-mode-remainder-4 r)
   (bnf-mode-remainder-5 r)))


(defun bnf-mode-update-error-value (e v)
  "error value"
  (bnf-mode-update-error e v (bnf-mode-bnf-location-from-error e)))

(defun bnf-mode-update-error (e v bnf-loc)
  (cond
   ((bnf-mode-is-error-group-ref-expected-buildin? e)
    (bnf-mode-make-error-group-ref-expected-buildin
     v
     bnf-loc
     (bnf-mode-error-group-ref-expected-buildin-1 e)))

   ((bnf-mode-is-error-constant? e)
    (bnf-mode-make-error-constant
     v
     bnf-loc
     (bnf-mode-error-constant-1 e)))

   ((bnf-mode-is-error-no-match-in-reference? e)
    (bnf-mode-make-error-no-match-in-reference
     v
     bnf-loc
     (bnf-mode-error-no-match-in-reference-1 e)))

   ((bnf-mode-is-error-unmatched-input? e)
    (bnf-mode-make-error-unmatched-input
     v
     bnf-loc))

   ((bnf-mode-is-error-arity? e)
    (bnf-mode-make-error-arity
     v
     bnf-loc
     (bnf-mode-error-arity-1 e)
     (bnf-mode-error-arity-2 e)))


   ;; ((bnf-mode-is-error-missing-paren? e)
   ;;  (bnf-mode-make-error-missing-paren
   ;;   v
   ;;   bnf-loc))

   ((bnf-mode-is-error-plus-at-least-one-match? e)
    e)


   ;; A missing syntax error has special meaning,
   ;; any missing syntax error should be reported
   ;; as it is.
   ((bnf-mode-is-error-missing-syntax? e)
    e)


   ((bnf-mode-is-worst-possible-error? e)
    e)
   

   ((bnf-mode-is-error-missing-paren? e)
    (bnf-mode-make-error-missing-paren
     v
     bnf-loc))

   ((bnf-mode-is-error-group-ref-not-found? e)
    (bnf-mode-make-error-group-ref-not-found
     v
     bnf-loc
     (bnf-mode-error-group-ref-not-found-1 e)))

   ;; (t
   ;;  e)
   
   (t 
    (error "update-error: TODO implement rest: %s %s %s" e v bnf-loc))
   ))


(defun bnf-mode-handle-possible-error (r bnf-reference constant-context)
  (if (bnf-mode-is-remainder-true? r)
      r
  (cond

   ((or (bnf-mode-is-error-missing-paren? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	(bnf-mode-is-error-group-ref-expected-buildin? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	(bnf-mode-is-error-arity? (bnf-mode-false-1 (bnf-mode-remainder-1 r)))
	)

    (let ((res
	   (bnf-mode-make-remainder
	    (bnf-mode-false
	     (bnf-mode-make-error-no-match-in-reference
	      (bnf-mode-extract-nested-value-if-necessary r (bnf-mode-remainder-2 r))
	      (bnf-mode-extract-nested-bnf-if-necessary r bnf-reference)
	      (bnf-mode-false-1 (bnf-mode-remainder-1 r))))
	    (bnf-mode-remainder-2 r)
	    (bnf-mode-remainder-3 r)
	    (bnf-mode-remainder-4 r)
	    (bnf-mode-remainder-5 r))))

      res
      ))

   (t
    r)

)))


;; walks through the top members
(defun bnf-mode-handle-members (member-list v reference-bnf)
  (bnf-mode-fold-left
   member-list
   (lambda (bnf-mode-x rest) 
     (cond

      ((bnf-mode-has-constant? rest)
       rest)

      ((bnf-mode-is-remainder-match? rest)
       rest)
      
      (t
       (let ((new-member-match
	      (bnf-mode-check-bnf 
	       bnf-mode-x
	       v 
	       reference-bnf)))
	 (bnf-mode-ensure-progress
	  new-member-match 
	  rest
	  bnf-mode-x)))))

   (lambda () 
     (bnf-mode-make-remainder
      (bnf-mode-false (bnf-mode-make-worst-possible-error))
      v
      nil
      nil
      nil))))


;; bnf * list of input syntax * bnf --> remainder of boolean * list of input syntax
(defun bnf-mode-check-bnf (bnf v reference-bnf)
  (cond

   ((bnf-mode-is-members? bnf)
    (bnf-mode-handle-members (bnf-mode-members-1 bnf) v reference-bnf))


   ;; Check the content of a member's values for possible multipliers.
   ((bnf-mode-is-member? bnf)
    (let ((r
	   (if (bnf-mode-has-member-multiplier? (bnf-mode-member-1 bnf))
	       (bnf-mode-handle-member-with-multiplier (bnf-mode-member-1 bnf) v reference-bnf)
	     (bnf-mode-handle-member-without-multiplier (bnf-mode-member-1 bnf) v reference-bnf nil nil))))
      (if (bnf-mode-is-remainder-partly-match? r)
	  (bnf-mode-update-remainder-with-new-error
	   r
	   (bnf-mode-make-error-unmatched-input 
	    (bnf-mode-remainder-2 r)
	    (bnf-mode-extended-bnf-location-from-value (bnf-mode-member-1 bnf))))
	r)))


   ((bnf-mode-is-bnf-match? bnf v)
    (bnf-mode-make-remainder (bnf-mode-true) v nil nil nil))

   (t
    (error "Unhandled case: %s %s" bnf v))

   ))

(defun bnf-mode-check-bnf-main (bnf v)
  ;; Peel off the top layers and start from the bnf-mode-first group, which is
  ;; assumed to be the top level of the grammar.
  (bnf-mode-reset-bnf-key-words bnf)
  (bnf-mode-remainder-1 
   (bnf-mode-check-bnf 
    (bnf-mode-group-2 (car (bnf-mode-groups-1 (bnf-mode-bnf-1 bnf))))
    v 
    bnf)))

(defun bnf-mode-extended-bnf-location-from-value (v)
  (cond
   ((bnf-mode-is-constructor? v)
    (bnf-mode-bnf-location-from-value v))

   (t
    (bnf-mode-extend-bnf-location
     (bnf-mode-bnf-location-from-value (car v))
     (bnf-mode-bnf-location-from-value (bnf-mode-last-element-of-list v))))))




(defun bnf-mode-syntax-location-from-error (v)
  (cond
   ((bnf-mode-is-error-paren-type-mismatch-left? v)
    (let ((left (bnf-mode-syntax-location-1 
   		 (bnf-mode-syntax-location-from-value 
   		  (bnf-mode-value-from-error v)))))
      (bnf-mode-make-syntax-location
       left
       (+ 1 left))))

   ((bnf-mode-is-error-paren-type-mismatch-right? v)
    (let ((right (bnf-mode-syntax-location-2
   		  (bnf-mode-syntax-location-from-value 
   		   (bnf-mode-value-from-error v)))))
      (bnf-mode-make-syntax-location
       (- right 1)
       right)))
     

   ((bnf-mode-is-build-in-error? v)
    (bnf-mode-syntax-location-from-value
     (bnf-mode-value-from-build-in-error v)))

   ((bnf-mode-is-error-group-ref-expected-buildin? v)
    (if (bnf-mode-is-error? (bnf-mode-error-group-ref-expected-buildin-1 v))
	(bnf-mode-syntax-location-from-error
	 (bnf-mode-error-group-ref-expected-buildin-1 v))
      (bnf-mode-syntax-location-from-error v)))
   
   (t
    (let ((syntax (bnf-mode-value-from-error v)))
      (if (bnf-mode-is-constructor? syntax)
	  (bnf-mode-syntax-location-from-value syntax)
  
	(if (< 1 (length syntax))
	    (bnf-mode-extend-syntax-location
	     (bnf-mode-syntax-location-from-value (car syntax))
	     (bnf-mode-syntax-location-from-value (bnf-mode-last-element-of-list syntax)))
	  (if (= 1 (length syntax))
	      (bnf-mode-syntax-location-from-value (car syntax))
	    (if (bnf-mode-is-error-no-match-in-reference? v)
		(bnf-mode-syntax-location-from-error 
		 (bnf-mode-error-no-match-in-reference-1 v))
	      (message "missing input syntax")))))))))

(defun bnf-mode-bnf-mode-read-buffer (buffer)
  (with-current-buffer buffer
    (set-text-properties 1 (point-max)  nil buffer)
    (buffer-string)))


(defun bnf-mode-clear-bnf-buffer-underlining ()
  (let ((buffer (get-buffer-create "*bnf-mode-bnf*")))
    (with-current-buffer buffer
      (set-text-properties 1 (point-max)  nil buffer)
      (buffer-string))))



(defun bnf-mode-bnf-mode-point-to-error (location buffer)
  (with-current-buffer buffer
    (let ((left (+ 1 (bnf-mode-syntax-location-1 location)))
	  (right (+ 1 (bnf-mode-syntax-location-2 location))))
      (add-text-properties  left right '(face ((:underline "#ff0000") bold)) buffer)
      )))
  


(defvar bnf-mode-buffers-initialized nil)
(defun bnf-mode-setup-bnf-buffers ()
  (if (not bnf-mode-buffers-initialized)
      (let ((input-buf (current-buffer))
	    (bnf-buf (get-buffer-create "*bnf-mode-bnf*")))	
	(with-current-buffer bnf-buf
	  (if (and (= 0 (length (buffer-string)))
		   (car (file-expand-wildcards "*.bnf")))
	      (insert (bnf-mode-file-string (car (file-expand-wildcards "*.bnf"))))))
	(pop-to-buffer bnf-buf)
	(pop-to-buffer input-buf)
	(setq bnf-mode-buffers-initialized t)
    )))



(defun bnf-mode-bnf-loc-to-text (bnf-loc buf)
  (with-current-buffer buf
    (buffer-substring
     (+ 1 (bnf-mode-bnf-location-1 bnf-loc))
     (+ 1 (bnf-mode-bnf-location-2 bnf-loc)))))

(defun bnf-mode-bnf-check ()
  "Runs the grammar checker on the current buffer"
  (interactive)
  (message "BNF: Analyzing...")
  (let* ((input-buf (get-buffer (current-buffer)))
	(bnf-buf (get-buffer-create "*bnf-mode-bnf*"))
	(input-parsed (bnf-mode-sexp-parse-main (bnf-mode-bnf-mode-read-buffer input-buf)))
	(bnf-parsed (bnf-mode-bnf-parse-main (bnf-mode-bnf-mode-read-buffer bnf-buf))))
    
    (let ((res (bnf-mode-check-bnf-main
		bnf-parsed
		input-parsed)))
      
      (if (bnf-mode-is-false? res)
	  (progn

	    (let ((input-location (bnf-mode-syntax-location-from-error (bnf-mode-false-1 res)))
		  (bnf-location (bnf-mode-bnf-location-from-error (bnf-mode-false-1 res))))
	      (message "BNF: Error in BNF at: %s" (bnf-mode-bnf-loc-to-text bnf-location bnf-buf))
	      
	    (if (not (null bnf-location))
		(bnf-mode-handle-bnf-error-location 
		 bnf-location 
		 bnf-buf))
	    (if (not (null input-location))
		(bnf-mode-bnf-mode-point-to-error input-location input-buf))))
	(message "BNF: Correct!")))))





(defun bnf-mode-underline-text (text syntax-location)
  (add-text-properties 
   (bnf-mode-syntax-location-1 syntax-location)
   (bnf-mode-syntax-location-2 syntax-location)
   '(face ((:underline "#ff0000") bold)) text))

(defun bnf-mode-bnf-check-analysis-region (sexp)
  "Runs the grammar checker on the given sexp"
  (message "BNF: Analyzing...")
  (let* ((bnf-buf (get-buffer-create "*bnf-mode-bnf*"))
	 (input-parsed (bnf-mode-sexp-parse-main sexp))
	 (bnf-parsed (bnf-mode-bnf-parse-main (bnf-mode-bnf-mode-read-buffer bnf-buf))))
    
    (let ((res (bnf-mode-check-bnf-main
		bnf-parsed
		input-parsed)))
      ;(message "%s" res)
      (if (bnf-mode-is-false? res)
	  (let ((input-location (bnf-mode-syntax-location-from-error (bnf-mode-false-1 res)))
		(bnf-location (bnf-mode-bnf-location-from-error (bnf-mode-false-1 res))))
	    (message "BNF: Error in BNF at: %s" (bnf-mode-bnf-loc-to-text bnf-location bnf-buf))
	    (if (not (null bnf-location))
		(bnf-mode-handle-bnf-error-location 
		 bnf-location 
		 bnf-buf))
	    (if (not (null input-location))
		(bnf-mode-handle-syntax-error-location input-location (current-buffer) sexp)))
	(message "BNF: Correct!"))
      res)))


(defun bnf-mode-handle-bnf-error-location (loc bnf-buf)
  (bnf-mode-point-to-location loc bnf-buf)
  (bnf-mode-bnf-mode-point-to-error bnf-location bnf-buf))


(defun bnf-mode-handle-syntax-error-location (loc syntax-buf sexp)
  (bnf-mode-underline-text sexp loc))
  
(defun bnf-mode-point-to-location (loc buf)
  (set-window-point
   (get-buffer-window buf)
   (bnf-mode-bnf-location-1 loc)))


;; (defun bnf-mode-x ()
;; ""
;;   (interactive)
;; ;  (end-of-line)
;;   (search-backward-regexp "\w\\|)"))


(defvar bnf-mode-analysis-active nil)
(defun bnf-mode-bnf-check-region ()
  "temp function for testing what `thing-at-point' returns"
  (interactive)
;  (end-of-line)
  (search-backward-regexp "\\w\\|)") 
      
 ; 
  (let* ((myresult (thing-at-point 'sexp))
	(region (bounds-of-thing-at-point 'sexp))
	(cursor-point (point)))
    
    (if bnf-mode-analysis-active
      (progn 
	(setq bnf-mode-analysis-active nil)
    	(bnf-mode))
      (progn 
	(if (bnf-mode-is-false? (bnf-mode-bnf-check-analysis-region myresult))
	    (progn
	      (bnf-fundamental-mode)
	      (setq bnf-mode-analysis-active t)
	
	      (delete-region (car region) (cdr region))
	      (goto-char (car region))
;	      (insert-before-markers myresult)
	      (insert myresult)
	  
	      (goto-char cursor-point)
	      ))))
    (forward-char 1)
    ))



(define-derived-mode bnf-fundamental-mode
  fundamental-mode
;  petite-chez-scheme-mode
;  prog-mode
;  sml-mode
;  scheme-mode
  "BNF mode: Error Environment"
  "bnf mode for dProgSprog"
  (define-key bnf-fundamental-mode-map (bnf-mode-bnf-mode-activation-key) 'bnf-mode-bnf-check-region))


(define-derived-mode bnf-mode
;  fundamental-mode 
;  petite-chez-scheme-mode
;  prog-mode
;  sml-mode
  scheme-mode
  "BNF mode: Scheme"
  "bnf mode for dProgSprog"
  (bnf-mode-clear-bnf-buffer-underlining)
  (define-key bnf-mode-map (bnf-mode-bnf-mode-activation-key) 'bnf-mode-bnf-check-region)
  (bnf-mode-setup-bnf-buffers))



;; Updates to the latest version, and loads the bnf-mode-update
(defun bnf-mode-update ()
  "Update the bnf mode to latest version. The bnf-mode.el file is placed at:  ~/.emacs.d/bnf-mode.el and loaded from there."
  (interactive)
  (url-copy-file "http://users-cs.au.dk/u061245/thesis/latest/bnf-mode.el" "~/.emacs.d/bnf-mode.el" t)
  (load-file "~/.emacs.d/bnf-mode.el"))


