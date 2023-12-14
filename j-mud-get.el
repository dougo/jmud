(defvar mud-prefix-id (prin1-to-string (random))
  "*Random number generated at load time to identify un-spoofed PREFIXes.")

(defun mud-get-text (object &optional fixer)
  "Fetch the output of a given MUD command."
  (interactive "sCommand: ")
  (if (and (not (equal object ""))
	   (mud-make-fetch-buffer object t))
      (mud-do-fetch object (or fixer "") object)))

(defun mud-fix-unknown ()
  (if (looking-at "^$")
      (progn
	(delete-char 1)
	(end-of-buffer)
	(forward-line -1)
	(if (looking-at "^\\.$")
	    (delete-char 2))))
  (goto-char (point-min))
  (mud-copy-here-from buff)
  (mud-macro-expansion-mode)
;  (toggle-read-only)
  )

(defun mud-get-long-result (cmd)
  "Execute a given command and catch the results in a separate buffer."
  (interactive "sCommand: ")
  (if (mud-make-fetch-buffer cmd t)
      (mud-do-fetch cmd "" cmd t)))

(defun mud-do-fetch (command mode buffer &optional open-ended suspending)
  (mud-send-here (if open-ended
		     (concat ";notify(player,\"~" mode "~" buffer "~~"
			     mud-prefix-id "~\")\n" command "\n")
		   (concat "PREFIX #$# edit name: " buffer " upload: "
			   " type: " mode "\n"
			   "SUFFIX .\n"
			   command
			   "\nPREFIX\nSUFFIX\n"))))
  
(defun mud-do-fetch (command mode buffer &optional open-ended suspending)
  (mud-send-here (if open-ended
		     (concat ";notify(player,\"~" mode "~" buffer "~~"
			     mud-prefix-id "~\")\n" command "\n")
		   (concat "PREFIX ~" mode "~" buffer "~~"
			   mud-prefix-id "~\n"
			   "SUFFIX ~end~" mode "~" buffer "~~"
			   mud-prefix-id "~"
			   (if suspending "suspend")
			   "\n" command
			   "\nPREFIX\nSUFFIX\n"))))

(defun mud-make-fetch-buffer (name pop-up)
  (let ((buf (current-buffer)))
    (if pop-up
	(pop-to-buffer (get-buffer-create name))
      (set-buffer (get-buffer-create name)))
    (if (and pop-up
	     (or (equal (buffer-string) "")
		 (y-or-n-p (concat "Erase buffer " name "? "))))
	(progn
	  (erase-buffer)
	  (mud-copy-here-from buf))
      nil)))

(defun mud-output-here ()
  (interactive)
  (put mud-here 'output-buffer (current-buffer))
  (put mud-here 'redirect-function (function mud-redirect-output))
  (setq process-mark (point-max)))

(defun mud-redirect-output (line)
  (save-excursion
    (let ((buff (current-buffer)))
      (cond ((eq (mud-output-buffer) buff)
	     (cond ((eq (string-match
			 (concat "~\\(.*\\)~\\(.+\\)~\\(.*\\)~"
				 mud-prefix-id "~$") line)
			0)
		    (put mud-here 'output-buffer (mud-match-string 2 line)))
		   ((and (boundp 'mcp-message-regexp)
			 (eq (string-match mcp-end-regexp line) 0))
		    (mcp-handle-end (mud-match-string 1 line)))
		   ((and (boundp 'mcp-message-regexp)
			 (eq (string-match mcp-message-regexp line) 0))
		    (mcp-handle-message (mud-match-string 1 line)
					(if (match-beginning 3)
					    (mud-match-string 3 line)
					  nil)
					(mud-match-string 5 line)))
		   ((and (boundp 'mcp-message-regexp)
			 (eq (string-match mcp-multifield-regexp line) 0))
		    (mcp-handle-multifield (mud-match-string 1 line)
					   (mud-match-string 2 line)
					   (mud-match-string 3 line)))
		   ((or (eq (string-match (concat "#\\$# edit "
						  "name: \\(.*\\) "
						  "upload: \\(.*\\) "
						  "type: \\(.*\\)$")
					  line) 0)
			(eq (string-match (concat "#\\$# edit "
						  "name: \\(.*\\) "
						  "upload: \\(.*\\)$")
					  line) 0))
		    (let ((name (mud-match-string 1 line))
			  (upload (mud-match-string 2 line))
			  (mud-here-temp mud-here))
		      (put mud-here 'output-buffer
			   (mud-find-buffer mud-here name upload))
		      (put mud-here 'fixer-type
			   (cond ((match-end 3)
				  (setq barr (mud-match-string 3 line)))
				 ((not (zerop (length upload)))
				  (let ((space (string-match " " 
							     upload)))
				    (if space
					(substring upload 0 space)
				      upload)))
				 (t
				  "unknown")))
			(put mud-here 'redirect-function
			     (function mud-redirect-local-edit))
			(set-buffer (mud-output-buffer))
			(setq mud-here mud-here-temp)
			(erase-buffer)
			(insert upload "\n")
			(set-buffer buff)))
		   ((and (boundp 'jtext-mode)
			 jtext-mode
			 (string-match "^lemootag: \\(.*\\)$" line))
		    (jtext-insert-vbox (read (mud-match-string 1 line))))
		   ((and (mud-world-get mud-here 'gag-regexp)
			 (eq 0 (string-match (mud-world-get mud-here
							    'gag-regexp)
					     line)))
		    ;;; don't insert the line, bwahaha
		    )
		   (t
		    (insert-before-markers line "\n"))))
	    ((and (mud-world-get mud-here 'error-string)
		  (eq 0 (string-match (mud-world-get mud-here 'error-string)
				      line)))
	     (insert-before-markers line "\n")
	     (pop-to-buffer (mud-output-buffer))
	     (put mud-here 'output-buffer buff)
	     (message "Server error in %s" (mud-match-string 0 line))
	     (set-buffer buff))
	    ((and (mud-world-get mud-here 'error-string)
		  (eq 0 (string-match "\\(~+\\)#[0-9]+:"
				      line)))
	     (delete-region start (point))
	     (set-buffer (mud-output-buffer))
	     (goto-char (point-max))
	     (insert (substring line
				(1+ (length (mud-match-string 1 line)))))
	     (newline)
	     (set-buffer buff))
	    ((eq 0 (string-match (concat "~end~\\(.*\\)~"
					 "\\(.*\\)~"
					 "\\(.*\\)~"
					 mud-prefix-id
					 "~\\(suspend\\)?$") line))
	     (let ((done (not (match-end 4))))
	       (set-buffer (mud-output-buffer))
	       (put mud-here 'output-buffer buff)
	       (goto-char (point-min))
	       (if done (funcall (mud-fixer mud-here
					    (mud-match-string 1 line))))
	       (set-buffer buff)))
	    (t
	     (delete-region start (point))
	     (set-buffer (mud-output-buffer))
	     (goto-char (point-max))
	     (insert line)
	     (newline)
	     (set-buffer buff))))))

(put (mud-world "global") 'redirect-function (function mud-redirect-output))

(defvar mud-redirector (function mud-redirect-output)
  "Function called on each line of text.")
(make-variable-buffer-local 'mud-redirector)

(defun mud-find-buffer (world name upload)
  "Find or create a buffer for WORLD with name NAME or upload command UPLOAD.
This is complicated because we want to replace a buffer named NAME iff it's from the same world.  We may use UPLOAD, if it's a kind of info we keep track of."
  (let* ((info (cond ((string-match "@upload-obj #\\([0-9]+\\)" upload)
		      (list (moo-make-object world (string-to-int
						    (mud-match-string 1
								      upload)))
			    moo-object-buffers))))
	 (buffer (and info
		      (apply 'moo-which-buffer info))))
    (if (and buffer (buffer-name buffer))
	buffer
      (generate-new-buffer name))))
	  
(defun mud-redirect-local-edit (line)
  (save-excursion
    (let ((buff (current-buffer)))
      (cond ((and (mud-world-get mud-here 'error-string)
		  (eq 0 (string-match (mud-world-get mud-here 'error-string)
				      line)))
	     (insert-before-markers line "\n")
	     (pop-to-buffer (mud-output-buffer))
	     (put mud-here 'output-buffer buff)
	     (message "Server error in %s" (mud-match-string 0 line))
	     (set-buffer buff))
	    ((eq 0 (string-match "^\\.\\(.+\\)$" line))
	     (delete-region start (point))
	     (set-buffer (mud-output-buffer))
	     (goto-char (point-max))
	     (insert (mud-match-string 1 line))
	     (newline)
	     (set-buffer buff))
	    ((string-match "^\\.$" line)
	     (set-buffer (setq mud-goto-buffer (mud-output-buffer)))
	     (put mud-here 'output-buffer buff)
	     (put mud-here 'redirect-function (function mud-redirect-output))
	     (insert line "\n")
	     (goto-char (point-min))
	     (funcall (mud-fixer mud-here (mud-world-get
					   mud-here
					   'fixer-type)))
	     (set-buffer-modified-p nil)
	     (set-buffer buff))
	    (t
	     (delete-region start (point))
	     (set-buffer (mud-output-buffer))
	     (goto-char (point-max))
	     (insert line)
	     (newline)
	     (set-buffer buff))))))


(defvar mud-buffer-info '() "Alist of what buffers contain what information.")

(defun mud-mark-association (key value alist)
  "Mark a given KEY-VALUE combination in the alist denoted by the symbol ALIST.
Return the information previously associated with KEY, or nil if we have no prior knowledge of it."
  (let ((cur (assoc key (symbol-value alist))))
    (if cur
	(prog1
	    (cdr cur)
	  (setcdr cur value))
      (set alist
	   (cons (cons key value) (symbol-value alist)))
      nil)))

(defun mud-remove-association (key alist)
  "Remove a given KEY from the alist denoted by the symbol ALIST.
Return the information previously associated with KEY."
  (let ((cur (assoc key (symbol-value alist))))
    (if cur
	(prog1
	    (cdr cur)
	  (set alist
	       (delq cur (symbol-value alist)))))))
  

(defun mud-buffer-has (buffer info)
  "Note that the contents of BUFFER refer to INFO.
INFO can be anything, and is interpreted on a MUD-specific basis."
  (mud-mark-association buffer info 'mud-buffer-info))

(defun mud-buffer-unhas (buffer)
  "Note that BUFFER no longer contains useful information."
  (mud-remove-association buffer 'mud-buffer-info))

(defun mud-info-for-buffer (buffer)
  "What does BUFFER contain, MUDwise?"
  (let ((cur (assoc buffer mud-buffer-info)))
    (if cur
	(cdr cur)
      nil)))

(provide 'j-mud-get)
