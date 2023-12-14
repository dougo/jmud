;;; Tree-browsing mode for MOOs
(provide 'j-boom-tree)

(defvar boom-tree-mode-map nil "")

(if boom-tree-mode-map
    ()
  (setq boom-tree-mode-map (make-sparse-keymap))
  (let ((map boom-tree-mode-map))
    (define-key map "\t"       'back-to-indentation);
    (define-key map "\C-n"     'boom-tree-next-visible-object);
    (define-key map "n"        'boom-tree-next-sibling);
    (define-key map "f"        'boom-tree-next-sibling);
    (define-key map "\C-p"     'boom-tree-previous-visible-object);
    (define-key map "p"        'boom-tree-previous-sibling);
    (define-key map "b"        'boom-tree-previous-sibling);
    (define-key map "u"        'boom-tree-up-level);
    (define-key map "h"        'boom-tree-hide-subtree);
    (define-key map "i"        'boom-tree-show-children)
    (define-key map "s"        'boom-tree-show-subtree);
    (define-key map "r"        'boom-tree-rename-object)
    (define-key map "g"        'moo-get-boom-tree-object);
    (define-key map "c"        'moo-get-boom-tree-children)
    (define-key map "j"        'boom-tree-jump-to-object);
)
)

(defun boom-tree-mode ()
  "Foo."
  (interactive)
  (kill-all-local-variables)
  (setq selective-display t)
  (use-local-map boom-tree-mode-map)
  (setq mode-name "BOOM Tree")
  (setq major-mode 'boom-tree-mode)
  (setq indent-line-function 'back-to-indentation))

(defun boom-tree-objnum ()
  (save-excursion
    (back-to-indentation)
    (if (looking-at ".* (\\(.*\\))$")
	(mud-match 1))))

(defun boom-tree-name ()
  (save-excursion
    (back-to-indentation)
    (if (looking-at "\\(.*\\) (#[0-9]*)$")
	(mud-match 1))))

(defun boom-tree-parent ()
  (save-excursion
    (boom-tree-up-level 1)
    (boom-tree-objnum)))

(defun boom-tree-jump-to-object (object)
  (interactive "sObject: ")
  (setq object (regexp-quote object))
  (if (eq (elt object 0) ?#)
      (setq object (concat "^.* (" object ")$")))
  (save-excursion
    (if (search-forward object nil t)
	(setq object (match-beginning 0))
      (goto-char (point-min))
      (if (search-forward object nil t)
	  (setq object (match-beginning 0)))))
  (cond ((integerp object)
	 (goto-char object)
	 (back-to-indentation)
	 t)
	((interactive-p)
	 (error "Object not found."))))

(defun boom-tree-next-visible-object ()
  "Foo."
  (interactive)
  (forward-line 1)
  (back-to-indentation))

(defun boom-tree-previous-visible-object ()
  "Foo."
  (interactive)
  (forward-line -1)
  (back-to-indentation))

(defun boom-tree-next-sibling (arg)
  "Moves forward to the ARG'th object from here that is a sibling of the
current object."
  (interactive "p")
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion (boom-tree-get-next-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(setq arg 0)
	(error "")))))

(defun boom-tree-get-next-sibling ()
  "Position the point at the next object, and return that position or
nil if it cannot be found."
  (let ((level (current-indentation)))
    (forward-line 1)
    (while (and (> (current-indentation) level)
		(not (eobp)))
      (forward-line 1))
    (if (or (< (current-indentation) level) (eobp))
	nil
      (back-to-indentation)
      (point))))

(defun boom-tree-previous-sibling (arg)
  "Move backward to the ARG'th object from here that is a sibling of the current
object."
  (interactive "p")
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion (boom-tree-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(setq arg 0)
	(error "")))))
	    
(defun boom-tree-get-last-sibling ()
  "Position the point at the previous sibling of the current object,
and return that position or nil if it cannot be found."
  (let ((level (current-indentation)))
    (forward-line -1)
    (while (and (> (current-indentation) level)
		(not (bobp)))
      (forward-line -1))
    (if (< (current-indentation) level)
	nil
      (back-to-indentation)
      (point))))

(defun boom-tree-up-level (arg)
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (if (and (interactive-p) (zerop (current-indentation)))
      (error "")
    (while (and (> (current-indentation) 0)
		(> arg 0)
		(not (bobp)))
      (let ((present-level (current-indentation)))
	(while (not (< (current-indentation) present-level))
	  (forward-line -1))
	(setq arg (1- arg))))
    (back-to-indentation)))
  
(defun boom-tree-show-all ()
  "Show all of the objects in the tree."
  (interactive)
  (boom-tree-flag-region (point-min) (point-max) ?\n))

(defun boom-tree-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `\\n' (newline character) then text is shown;
if FLAG is `\\^M' (control-M) the text is hidden."
  (let ((modp (buffer-modified-p)))
    (unwind-protect
	(subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t)
      (set-buffer-modified-p modp))))

(defun boom-tree-hide-subtree ()
  "Hide all descendants of this object."
  (interactive)
  (boom-tree-flag-subtree ?\^M))

(defun boom-tree-flag-subtree (flag)
  (save-excursion
    (boom-tree-flag-region (point)
			  (progn (boom-tree-end-of-subtree) (point))
			  flag)))

(defun boom-tree-end-of-subtree ()
  (beginning-of-line)
  (let ((opoint (point))
	(first t)
	(level (current-indentation)))
    (while (and (not (eobp))
		(or first (> (boom-tree-level) level)))
      (setq first nil)
      (boom-tree-next-object))
    (forward-char -1)
    (if (memq (preceding-char) '(?\n ?\^M))
	(forward-char -1))))

(defun boom-tree-level ()
  "Return the level of nesting the current object is at.
Like current-indentation, but handles ^M."
  (- (save-excursion
       (re-search-forward "[^ ]" (save-excursion (end-of-line) (point)) t)
       (match-beginning 0))
     (point)))


(defun boom-tree-next-object ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "[\n\^M]") nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun boom-tree-show-subtree ()
  "Show all known descendants of this object."
  (interactive)
  (boom-tree-flag-subtree ?\n))

(defun boom-tree-show-children ())
(defun boom-tree-show-branches ())

(defun moo-get-boom-tree-node (object)
  (interactive "sObject to locate: ")
  (setq mud-object (concat " *BOOM-tree-locate-object: " object
			   " in " (symbol-name mud-here) "*"))
  (save-excursion
    (display-buffer
     (get-buffer-create (concat "*Browser: " (symbol-name mud-here) "*")))
    (mud-make-fetch-buffer mud-object nil))
  (mud-do-fetch (concat "@br-locate-object " object)
	       "boom-tree-node" mud-object))

(defun moo-fix-boom-tree-node ()
  (mud-copy-here-from buff)
  (goto-char (point-max))
  (let (nodes node parent
	(buf (concat "*Browser: " (symbol-name mud-here) "*")))
    (while (re-search-backward "^.+$" nil "limit")
      (setq nodes (cons (list (boom-tree-objnum) (boom-tree-name)) nodes)))
    (kill-buffer (current-buffer))
    (set-buffer buf)
    (if (not (eq major-mode 'boom-tree-mode))
	(boom-tree-mode))
    (while nodes
      (if (boom-tree-jump-to-object (car (car nodes)))
	  (progn
	    (setq node (car nodes))
	    (looking-at "\\( *\\)\\(.*\\) (#\\(.*\\))")
	    (if (not (string= (mud-match-string 2) (nth 1 node)))
		(replace-match (concat "\1" (regexp-quote (nth 1 node))
				       " (#\3)") t))
	    (if (not (string= (boom-tree-parent) parent))
		(boom-tree-reparent parent))
	    (setq parent (car node))
	    (setq nodes (cdr nodes)))
	(if parent (boom-tree-jump-to-object parent))
	(while nodes
	  (insert (format "%s  %s (%s)\n"
			  (make-string (current-indentation) ?\ )
			  (nth 1 (car nodes)) (car (car nodes))))
	  (setq nodes (cdr nodes))))))
  (if (eobp)
      (delete-backward-char 1)))

(defun moo-get-boom-tree-children ()
  (interactive)
  (if (not (eq major-mode 'boom-tree-mode))
      (error "Not in tree browser."))
  (setq object (boom-tree-objnum))
  (setq mud-object (concat " *BOOM-tree-children: " object
			   "@" (symbol-name mud-here) "*"))
  (mud-make-fetch-buffer mud-object nil)
  (mud-do-fetch (concat "@br-children " object) "boom-tree-children"))

(defun moo-fix-boom-tree-children ()
  (mud-copy-here-from buff)
  (goto-char (point-max))
  (let ((buf (concat "*Browser: " (symbol-name mud-here) "*")) object children)
    (while (re-search-backward "^.+$" nil "limit")
      (setq children (cons (mud-match 0)) children))
    (setq object (car children)
	  children (cdr children))
    (kill-buffer (current-buffer))
    (set-buffer buf)
    (goto-char (point-min))))

(defun moo-get-boom-@chparent (object new-parent)
  (interactive (list
		(read-string (concat
			      "Object to @chparent"
			      (if (eq major-mode 'boom-tree-mode)
				  (concat "(default "
					  (boom-tree-objnum)
					  ")"))
			      " : "))
		(read-string "New parent: ")))
  (setq mud-object (concat " *BOOM-chparent-object: " object
			   " to " new-parent "*"))
  (save-excursion
    (display-buffer
     (get-buffer-create (concat "*Browser: " (symbol-name mud-here) "*")))
    (mud-make-fetch-buffer mud-object nil))
  (mud-do-fetch (concat "@chparent " object " to " new-parent)
		"boom-@chparent"))
   
(defun moo-fix-boom-@chparent ()
  (mud-copy-here-from buff)
  (goto-char (point-max))
  (if (not (looking-at "Parent changed\."))
      (message (buffer-substring (point) (progn (end-of-line) (point))))
    (string-match " \*BOOM-chparent-object: \\(.*\\) to \\(.*\\)\*"
		  (buffer-name))
    (let ((buf (concat "*Browser: " (symbol-name mud-here) "*"))
	  (object (mud-match 1))
	  (new-parent (mud-match 2))
	  known-object known-parent)
      (save-excursion
	(set-buffer buf)
	(setq known-object (boom-tree-jump-to-object object))
	(setq known-parent (boom-tree-jump-to-object new-parent))
	(if known-parent (setq known-parent (copy-marker known-parent)))
	(if (null known-object)
	    (message "%s (unknown) chparented to %s%s."
		     object new-parent
		     (if known-parent "" " (unknown)"))
	  (let* ((subtree-start (progn (goto-char object)
				       (beginning-of-line)
				       (point)))
		 (old-indent (current-indentation)) diff-indent
		 (subtree-end (1+ (progn (boom-tree-end-of-subtree)
					 (point))))
		 (subtree (buffer-substring subtree-start subtree-end)))
	    (delete-region subtree-start subtree-end)
	    (if (null known-parent)
		(message "%s chparented to unknown object %s."
			 object new-parent)
	      (goto-char known-parent)
	      (setq diff-indent (- (current-indentation old-indent)))
	      (forward-line 1)
	      (setq subtree-start (point))
	      (insert subtree)
	      (setq subtree-end (point))
	      (if (natnump diff-indent)
		  (indent-rigidly subtree-start (point) diff-indent)
		(narrow-to-region subtree-start (point))
		(while (eq (forward-line -1) 0)
		  (delete-char (- diff-indent)))
		(widen))))))))
  (kill-buffer (current-buffer)))
