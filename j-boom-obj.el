(provide 'j-boom-obj)

(defun boom-obj-rename-object-in-buffer (buff old new)
  (if buff
      (save-excursion
	(set-buffer buff)
	(goto-char (point-min))
	(while (re-search-forward (concat "\W" 

      (if (setq buf (get-buffer (concat "*" objnum "@" (symbol-name mud-here) "*")))
	  (progn 
))))))))	      

(defun boom-or-concat (&optional first-string &rest strings)
  "Concat a number of strings into a regexp that matches any of them."
  (if first-string
      (concat "\\(" first-string
	      (apply 'concat
		     (mapcar (function
			      (lambda (str)
				(concat "\\|" str)))
			     strings))
	      "\\)")
    ""))
      
(defvar boom-obj-object-expr "#[0-9]+" "Matches an object.")
(defvar boom-obj-vname-expr "\\([^\" ]+\\|\"[^\"]*\"\\)"
  "Matches a verb name.")

(defvar boom-obj-verb-expr (concat "\\(" 
				   boom-obj-object-expr
				   "\\)[:;]"
				   boom-obj-vname-expr)
  "Matches an object-verb expression.")
(defvar boom-obj-verb-match-object 1)
(defvar boom-obj-verb-match-vname 2)

(defvar boom-obj-player-expr "[^ ]+")
(defvar boom-obj-owner-expr (concat "\\("
				    boom-obj-player-expr
				    "\\) (\\("
				    boom-obj-object-expr
				    "\\))"))
(defvar boom-obj-owner-match-player 1)
(defvar boom-obj-owner-match-object 2)

(defvar boom-obj-flag-expr (boom-or-concat
			    "readable"
			    "writeable"
			    "fertile"
			    "wizard"
			    "programmer"
			    "(player)"))

(defvar boom-obj-object-line-expr (concat "^\\(.*\\) ("
					  boom-obj-object-expr
					  ") \\[ \\("
					  boom-obj-flag-expr
					  " \\)*\\]$"))
(defvar boom-obj-owner-line-expr (concat "  Owned by \\("
					 boom-obj-player-expr
					 "\\) (\\("
					 boom-obj-object-expr
					 "\\))."))
(defvar boom-obj-parent-line-expr (concat "^  Child of \\(.*\\) (\\("
					  boom-obj-object-expr
					  "\\)).$"))
(defvar boom-obj-location-line-expr (concat "^  Location \\(.*\\) (\\("
					    boom-obj-object-expr
					    "\\)).$"))
(defvar boom-obj-verb-perms-expr "[rwxd ]+")
(defvar boom-obj-verb-args-expr
  (let ((arg (boom-or-concat "any" "none" "this"))
	(prep (boom-or-concat "with" "using"
			      "at" "to"
			      "in front of"
			      "in" "inside" "into"
			      "on top of" "on" "onto" "upon"
			      "out of" "from inside" "from"
			      "over"
			      "through"
			      "under" "underneath" "beneath"
			      "behind"
			      "beside"
			      "for" "about"
			      "is"
			      "as"
			      "off" "off of")))
    (concat arg " " prep " " arg))
  "Matches an argspec.")

(defvar boom-obj-verb-args-match-dobjspec 1)
(defvar boom-obj-verb-args-match-prepspec 2)
(defvar boom-obj-verb-args-match-iobjspec 3)

(defvar boom-obj-verb-line-expr (concat "\\([-+ ~]\\) *\\("
					boom-obj-verb-expr
					"\\) +\\("
					boom-obj-owner-expr
					"\\) +\\("
					boom-obj-verb-perms-expr
					"\\) +\\("
					boom-obj-verb-args-expr
					"\\)"))
(defvar boom-obj-verb-line-match-mark 1)
(defvar boom-obj-verb-line-match-verb 2)
(defvar boom-obj-verb-line-match-verb-object 3)
(defvar boom-obj-verb-line-match-vname 4)
(defvar boom-obj-verb-line-match-owner 5)
(defvar boom-obj-verb-line-match-owner-player 6)
(defvar boom-obj-verb-line-match-owner-object 7)
(defvar boom-obj-verb-line-match-perms 8)
(defvar boom-obj-verb-line-match-args 1)
(defvar boom-obj-verb-line-match-dobjspec 1)
(defvar boom-obj-verb-line-match-prepstr 1)
(defvar boom-obj-verb-line-match-iobjspec 1)

(defvar boom-obj-pname-expr boom-obj-vname-expr "Matches a property name.")
(defvar boom-obj-prop-expr (concat "\\("
				   boom-obj-object-expr
				   "[.,^]"
				   boom-obj-pname-expr
				   "\\)")
  "Matches an object-property expression.")
(defvar boom-obj-verb-or-prop-expr (concat "\\("
					   boom-obj-verb-expr
					   "\\|"
					   boom-obj-prop-expr
					   "\\)")
  "Matches an object-verb or object-property expression.")

(defvar boom-obj-mode-map nil "Local keymap for boom-obj-mode buffers.")
(if boom-obj-mode-map
    nil
  (setq boom-obj-mode-map (make-keymap))
  (suppress-keymap boom-obj-mode-map t)
  (define-key boom-obj-mode-map "q"    'boom-obj-quit)
  (define-key boom-obj-mode-map "r"    'boom-obj-rename)
  (define-key boom-obj-mode-map "\C-d" 'boom-obj-flag-deleted)
  (define-key boom-obj-mode-map "d"    'boom-obj-flag-deleted)
  (define-key boom-obj-mode-map "v"    'boom-obj-edit)
  (define-key boom-obj-mode-map "e"    'boom-obj-edit)
  (define-key boom-obj-mode-map "f"    'boom-obj-edit)  
  (define-key boom-obj-mode-map "\C-j" 'boom-obj-edit)
  (define-key boom-obj-mode-map "o"    'boom-obj-edit-other-window)
  (define-key boom-obj-mode-map "u"    'boom-obj-unflag)
  (define-key boom-obj-mode-map "x"    'boom-obj-send)
  (define-key boom-obj-mode-map "\177" 'boom-obj-backup-unflag)
  (define-key boom-obj-mode-map "?"    'boom-obj-summary)
  (define-key boom-obj-mode-map "c"    'boom-obj-copy)
  (define-key boom-obj-mode-map "h"    'describe-mode)
  (define-key boom-obj-mode-map " "    'boom-obj-next-line)
  (define-key boom-obj-mode-map "\C-n" 'boom-obj-next-line)
  (define-key boom-obj-mode-map "\C-p" 'boom-obj-previous-line)
  (define-key boom-obj-mode-map "n"    'boom-obj-next-line)
  (define-key boom-obj-mode-map "p"    'boom-obj-previous-line)
  (define-key boom-obj-mode-map "g"    'boom-obj-revert-buffer)
  (define-key boom-obj-mode-map "M"    'boom-obj-chmod)
  (define-key boom-obj-mode-map "P"    'boom-obj-chmod)
  (define-key boom-obj-mode-map "A"    'boom-obj-chargs)
  (define-key boom-obj-mode-map "O"    'boom-obj-chown))

(defun boom-obj-quit ()
  "Quit boom-obj-mode, after query."
  (if (or (not (buffer-modified-p))
	  (y-or-n-p "Abandon changes?"))
      (boom-obj-kill-buffer (current-buffer))))

(defun boom-obj-kill-buffer (buffer)
  "Kill a buffer and remove references to it from our lists of information."
  (moo-unhave-something buffer 'moo-object-buffers)
  (kill-buffer buffer))

(defun boom-obj-place-point ()
  "Put the point in the `proper' space on the line.
Arbitrarily, this is decreed to be the separator between object and verb/property, or at the beginning of the objnum in one of the header lines."
  (interactive)
  (beginning-of-line)
  (if (looking-at "[- +~] *#[0-9]+[:;.,^]")
      (progn
	(re-search-forward "[:;.,^]")
	(forward-char -1))
    (let ((start (point)))
      (end-of-line)
      (re-search-backward "#[0-9]+" start t))))

(defconst boom-obj-marks '((change ?~)
			   (add ?+)
			   (delete ?-)
			   (leave ?\ ))
  "Alist of symbols and their corresponding marks.")

(defun boom-obj-mark-line (symbol)
  (save-excursion
    (beginning-of-line)
    (if (and (looking-at boom-obj-verb-line-expr)
	     (boom-obj-mark-override symbol
				     (rassq (char-after (point))
					    boom-obj-marks)))
	(progn
	  (delete-char 1)
	  (insert (assq symbol boom-obj-marks))))))

(defvar boom-obj-mark-override-table
  '((add (add . nil)
	 (delete . error)
	 (change . error)
	 (leave . t))
    (delete (add . error)
	    (delete . nil)
	    (change . t)
	    (leave . t))
    (change (add . nil)
	    (delete . error)
	    (change . nil)
	    (leave . t))
    (leave (add . error)
	   (delete . error)
	   (change . t)
	   (leave . nil))))

(defun boom-obj-mark-override (new old)
  (let ((result (cdr (assq old
			   (cdr (assq new boom-obj-mark-override-table))))))
    (if (eq result 'error)
	(error "Can't change mark %s to %s" old new)
      result)))

(defun boom-obj-rename (new)
  "Rename the object, verb, or property on this line."
  (interactive (list
		(read-string "New name: "
			     (or (boom-obj-find-name)
				 (error "Can't rename here.")))))
  (boom-obj-mark-line 'change)
  (boom-obj-place-point)
  (cond ((looking-at (concat "\\([:;]\\)" boom-obj-vname-expr " +"))
	 (let ((md (match-data))
	       (start (eq (elt new 0) ?\"))
	       (end (eq (elt new (1- (length new))) ?\")))
	   (cond ((string-match " " new)
		  (cond ((not (and start end))
			 (if (not start)
			     (setq new (concat new "\"")))
			 (if (not end)
			     (setq new (concat "\"" new)))))
		  (if (string-match "\"" (substring new 1 -1))
		      (error "Can't have quotes in verb names.")))
		 (t
		  (cond ((and start end)
			 (setq new (substring new 1 -1)))
			((or start end)
			 (error "Can't have quotes in verb names.")))))
	   (store-match-data md)
	   (replace-match (setq foo (concat (mud-match 1)
				  new
				  (make-string (- 24 (length new)) ?\ )
				  " "))
			  t t))))
  (boom-obj-place-point))

(defun boom-obj-find-name ()
  "Find the name of the verb/property/name on this line."
  (cond ((looking-at (concat "[:;]" boom-obj-vname-expr))
	 (mud-match 1))
	((looking-at (concat "[.,^]" boom-obj-pname-expr))
	 (mud-match 1))
	((looking-at boom-obj-object-expr)
	 (save-excursion
	   (beginning-of-line)
	   (cond ((looking-at boom-obj-object-line-expr)
		  (mud-match 1)))))))
	   
(defun boom-obj-flag-deleted ())
(defun boom-obj-edit ())
(defun boom-obj-edit-other-window ())
(defun boom-obj-unflag ())
(defun boom-obj-send ())
(defun boom-obj-backup-unflag ())
(defun boom-obj-summary ())
(defun boom-obj-copy ())

(defun boom-obj-next-line (lines)
  "Move to next line and place point someplace useful."
  (interactive "p")
  (forward-line (or lines 1))
  (while (looking-at "^\\(\\|\\.\\|-+ finished -+\\)$")
    (forward-line -1))
  (boom-obj-place-point))

(defun boom-obj-previous-line (lines)
  "Move to next line and place point someplace useful."
  (interactive "p")
  (forward-line (- 0 (or lines 1)))
  (if (looking-at "^@upload-obj #[0-9]+$")
      (forward-line 1))
  (boom-obj-place-point))

(defun boom-obj-goto-max ()
  "Move to last useful line of buffer."
  (goto-char (point-max))
  (boom-obj-previous-line 3))

(defun boom-obj-goto-min ()
  "Move to first useful line of buffer."
  (goto-char (point-min))
  (boom-obj-next-line 1))

(defun boom-obj-end-of-buffer ()
  "Move to last useful line of buffer.  Sets mark."
  (interactive)
  (set-mark (point))
  (boom-obj-goto-max))

(defun boom-obj-beginning-of-buffer ()
  "Move to first useful line of buffer.  Sets mark."
  (interactive)
  (set-mark (point))
  (boom-obj-goto-min))

(defun boom-obj-revert-buffer ())
(defun boom-obj-chmod ())
(defun boom-obj-chargs ())
(defun boom-obj-chown ())
