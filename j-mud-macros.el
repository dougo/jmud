;;; Macro Commands
(require 'prefix)

(defvar mud-macro-commands-file "~/.jmud_macros")
(defvar mud-delete-macro-window nil
  "*If non-nil, delete window when aborting macro.")
(defvar mud-macro-commands-alist '(("" ("")))
  "Alist of macros (keyed by strings)")
;;;(defvar mud-trigger-list nil "Buffer-local (urgh) alist of triggers.")
;;;(make-variable-buffer-local 'mud-trigger-list)
(defvar mud-global-trigger-list nil "Alist of global triggers.")
(defvar mud-recoverable-macros 1
  "*Number of buffers to be buried by mud-macro-send-and-destroy.")
(defvar mud-recover-macro-list nil
  "List of buffers buried by mud-macro-send-and-destroy that can be recovered.
Size of list is determined by variable mud-recoverable-buffers, an integer.")
(defvar mud-expansion-macro-name "" "Name of current macro.")
(defvar mud-exploded nil
  "*If t, the buffer consists of an exploded list of strings.")
(make-variable-buffer-local 'mud-exploded)
(defvar mud-auto-implode nil
  "*If t, don't ask user before imploding an exploded buffer.")
(defvar mud-macro-modes '(mud-macro-expansion-mode)
  "*List of recognized macro modes.")

(defun mud-macro-field-from-text (text)
  (if (equal text "")
      "~"
    (car (read-from-string text))))

(defun mud-macro-enlist ()
  "Return (name arg-names fields).  When called, should be looking-at ~macro."
  (forward-char 7)
  (list (read (current-buffer))
	(read (current-buffer))
	(save-restriction
	  (if (not (bolp))
	      (forward-line))
	  (let ((start (point))
		mud-macro)
	    (goto-char (point-max))
	    (while (re-search-backward "~\\([^~]*\\)~\\([^~]*\\)" start t)
	      (setq mud-macro (cons (mud-macro-field-from-text (mud-match 1))
				    (cons (mud-match 2)
					  mud-macro)))
	      (narrow-to-region (point-min) (point)))
	    (cons (buffer-substring start (point)) mud-macro)))))

(defun mud-macro-field-to-text (field)
  (cond ((equal field "~")
	 "~~")
	((stringp field)
	 field)
	(t
	 (concat "~" (prin1-to-string field) "~"))))

(defun mud-macro-delist (macro)
  "Print a macro from a list as a human-readable string."
  (insert "~macro "
	  (prin1-to-string (car macro)) " "
	  (prin1-to-string (nth 1 macro)) "\n")
  (let ((fields (nth 2 macro)))
    (while fields
      (insert (mud-macro-field-to-text (car fields)))
      (setq fields (cdr fields)))))
	 
(defun mud-macro-delist-all ()
  "Convert all MUD macros from lists into human-readable strings."
  (let ((macro-list mud-macro-commands-alist))
    (while macro-list
      (mud-macro-delist (car macro-list))
      (insert "\n")
      (setq macro-list (cdr macro-list)))))

(defun mud-macro-print-alist (trigger-list)
  (insert "(")
  (while trigger-list
    (insert (prin1-to-string (car trigger-list)) "\n ")
    (setq trigger-list (cdr trigger-list)))
  (insert ")\n"))
    
(defun mud-macro-delist-world-triggers ()
  (let ((world-list (append mud-worlds nil))
	trigger-list world)
    (while world-list
      (cond ((and (symbolp (setq world (car world-list)))
		  (setq trigger-list (mud-trigger-list world)))
	     (insert "~triggers world " (symbol-name world) "\n")
	     (mud-macro-print-alist trigger-list)
	     (insert "")))
      (setq world-list (cdr world-list)))))

(defun mud-macro-delist-type-triggers ()
  (let ((type-list (append mud-types nil))
	trigger-list type)
    (while type-list
      (cond ((and (symbolp (setq type (car type-list)))
		  (setq trigger-list (mud-trigger-list type)))
	     (insert "~triggers type " (symbol-name type) "\n")
	     (mud-macro-print-alist trigger-list)
	     (insert "")))
      (setq type-list (cdr type-list)))))

(defun mud-macro-delist-global-triggers ()
  (if mud-global-trigger-list
      (progn
	(insert "~triggers global\n")
	(mud-macro-print-alist mud-global-trigger-list)
	(insert ""))))

(defun mud-trigger-field (field &optional argnames)
  (or (nth field mud-macro-args)
      (let ((diff (- field (length mud-macro-args))))
	(if (natnump diff)
	    (setq mud-macro-args
		  (append mud-macro-args
			  (make-list (1+ diff) nil))))
	(setcar (nthcdr field mud-macro-args)
		(read-string (concat (nth field (or argnames
						    (error "Invalid trigger")))))))))

(defun mud-macro-sendable (macro &optional mud-macro-args)
  (let ((fields (nth 2 macro))
	(argnames (nth 1 macro))
	(sendable "") field)
    (while fields
      (setq field (car fields))
      (setq sendable (concat sendable 
			     (cond
			      ((numberp field)
			       (mud-trigger-field field argnames))
			      ((stringp field)
			       field)
			      (t
			       (or (eval field) "")))))
      (setq fields (cdr fields)))
    sendable))

(defun mud-check-triggers ()
  "Search for lines that match trigger regexps.
For each line, call mud-check-triggers-on-line."
  (mud-check-triggers-on-line (mud-trigger-list)))

(defun mud-check-triggers-on-line (trigger-list)
  "TRIGGER-LIST is an alist of the form (action regexp).  If the current line
matches regexp, this function will perform various actions according to the
type of action:
  If a string, we perform the mud-macro of that name.
  Otherwise, action is evaluated."
  (while trigger-list
    (if (looking-at (nth 1 (car trigger-list)))
	(let* ((trigger (car (car trigger-list)))
	       (data (match-data))
	       (trigger-matches nil))
	  (while data
	    (setq trigger-matches (cons 
				   (buffer-substring (car data) 
						     (nth 1 data))
				   trigger-matches))
	    (setq data (nthcdr 2 data)))
	  (setq trigger-matches (reverse trigger-matches))
	  (cond ((stringp trigger)
		 (mud-send-here (mud-macro-sendable
				 (assoc trigger
					mud-macro-commands-alist)
				 trigger-matches)))
		(t
		 (eval trigger)))
	  (setq trigger-list nil))
      (setq trigger-list (cdr trigger-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mud-macro-expansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\^c\^s" 'mud-macro-send)
    (define-key map "\^c\^c" 'mud-macro-send-and-destroy)
    (define-key map "\^c\^]" 'mud-macro-abort)
    (define-key map "\^c\^r" 'mud-retarget)
    (define-key map "\^c\^l" 'mud-macro-label)
    (define-key map "\^c\^t" 'mud-get-text)
    (define-key map "\^c;"   'moo-finish-long-result)
    (define-key map "\M-'"  'prefix-region-with-string)
    (define-key map "\M-\"" 'prefix-region-with-quote)
    (define-key map "\M-:"  'prefix-region-with-pose)
    map)
  "Keymap for mud-macro-expansion-mode.")

(defun mud-macro-expansion-mode ()
  "Major Mode for mucking with MUD macro expansion.
Commands:
\\{mud-macro-expansion-mode-map}
"
  (interactive)
  (let ((here mud-here) hook)
    (kill-all-local-variables)
    (setq mode-name "MUD Macro Expansion")
    (setq major-mode 'mud-macro-expansion-mode)
    (set-syntax-table mud-mode-syntax-table)
    (setq mud-here here)
    (use-local-map mud-macro-expansion-mode-map)
    (make-local-variable 'mud-expansion-macro-name)
    (make-local-variable 'mode-line-process)
    (if (mud-macro-mode)
	(funcall (mud-macro-mode)))
    (message "Use ^C^S to send, ^C^C to send and destroy, ^C^] to abort...")))

(defun mud-macro-label ()
  "Define buffer as mud-macro."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond ((looking-at "~macro ")
	   (let* ((newval (mud-macro-enlist))
		  (name (car newval))
		  (oldval (assoc name mud-macro-commands-alist)))
	     (if oldval
		 (setcdr oldval (cdr newval))
	       (setq mud-macro-commands-alist
		     (cons newval mud-macro-commands-alist)))
	     (message "Macro %s" name)))
	  ((looking-at "~triggers \\(\\S-*\\)")
	   (let ((world (mud-world (mud-match 1))))
	     (forward-line)
	     (cond (world
		    (put world 'trigger-list (read (current-buffer)))
		    (message "World %s triggers defined" world))
		   (t
		    (error "World %s unknown" world)))))
	  ((looking-at "~triggers world ")
	   (forward-char 16)
	   (let ((world (intern-soft (symbol-name (read (current-buffer)))
				     mud-worlds)))
	     (if world
		 (progn
		   (put world 'trigger-list (read (current-buffer)))
		   (message "world %s triggers defined" world))
	       (error "World %s unknown" world))))
	  ((looking-at "~triggers type ")
	   (forward-char 15)
	   (let ((type (intern-soft (symbol-name (read (current-buffer)))
				    mud-types)))
	     (if type
		 (progn
		   (put type 'trigger-list (read (current-buffer)))
		   (message "mud type %s triggers defined" type))
	       (error "MUD type $s unknown" type))))
	  ((looking-at "~triggers global")
	   (forward-line)
	   (setq mud-global-trigger-list (read (current-buffer))))
	  (t
	   (error "No label given")))))

(defun mud-macro-abort ()
  "Abort macro expansion buffer."
  (interactive)
  (if (not (memq (current-buffer) mud-recover-macro-list))
      (setq mud-recover-macro-list
	    (cons (current-buffer)
		  mud-recover-macro-list))
    (let ((rec-rev (reverse mud-recover-macro-list)))
      (while (> (length rec-rev) mud-recoverable-macros)
	(kill-buffer (car rec-rev))
	(setq rec-rev (cdr rec-rev)))
      (setq mud-recover-macro-list (reverse rec-rev))))
  (bury-buffer)
  (if mud-delete-macro-window
      (delete-window)))

(defun mud-macro-send (&optional slow)
  "Send contents of macro expansion buffer."
  (interactive "P")
  (if (and mud-exploded (or mud-auto-implode
			   (y-or-n-p "This buffer is an exploded list of strings.  Implode it? ")))
      (moo-implode-list nil nil nil))
  (cond ((or (memq major-mode mud-macro-modes)
	     (y-or-n-p "This is not a recognized macro mode.  Send anyway?"))
	 (set-buffer-modified-p nil)
	 (save-excursion
	   (goto-char (point-min))
	   (let ((text
		  (if (looking-at "~macro ")
		      (mud-macro-sendable (mud-macro-enlist))
		    (buffer-string))))
	     (if slow
		 (mud-macro-send-slowly slow text)
	       (save-excursion
		 (mud-send-here text))))))))

(defun mud-macro-send-slowly (lines text)
  "Send contents of a macro expansion n lines at a time, with 1 sec between."
  (if (not (integerp lines))
      (setq lines 1))
  (let ((start 0)
	(match (mapconcat '(lambda (x) x) (make-list lines ".*\n") "")))
    (while (string-match match text start)
      (let ((end (match-end 0)))
	(process-send-string (mud-process)
			     (substring text start end))
;	(mud-send-here (substring sendable start end))
	(setq start end)
	(sit-for 1)))
    (mud-send-here (substring text start))))

(defun mud-macro-send-and-destroy ()
  "Send contents of macro expansion buffer and then kill the buffer."
  (interactive)
  (mud-macro-send)
  (mud-macro-abort))

(defun mud-recover-last-macro ()
  (interactive)
  (switch-to-buffer (car mud-recover-macro-list))
  (setq mud-recover-macro-list (cdr mud-recover-macro-list)))

(defun mud-retarget (world)
  "Change mud-here (target for information sent)."
  (interactive (list
		(mud-request-world-name)))
  (setq mud-here world)
  (let ((macro-mode (mud-macro-mode)))
    (if (and (not (eq major-mode 'moo-code-mode))
	     macro-mode)
	(funcall macro-mode)))
  (if (assq 'mode-line-process (buffer-local-variables))
      (let ((s (concat "@" (symbol-name mud-here))))
	(setq mode-line-process
	      (if (> (length s) 20) (substring s 0 20) s)))))

(defun mud-load-macro-commands (filename)
  "Load file of mud-macros."
  (interactive (list (expand-file-name
		      (read-file-name (concat "File to load from (default "
					      mud-macro-commands-file
					      "): ")
				      "~/"
				      mud-macro-commands-file))))
  (if (file-exists-p filename)
      (let (last)
	(setq mud-macro-commands-file filename)
	(set-buffer (get-buffer-create " *MUD Macros*"))
	(erase-buffer)
	(insert-file-contents filename)
	(narrow-to-page)
	(while (not (eq last (point)))
	  (setq last (point))
	  (mud-macro-label)
	  (narrow-to-page 1))
	(message "Done")
	"(kill-buffer (current-buffer))")
    (error "File not found: %s" filename)))

(defun mud-store-macro-commands (filename)
  "Store MUD macros in FILENAME."
  (interactive (list (expand-file-name
		      (read-file-name (concat "File to save to (default "
					      mud-macro-commands-file
					      "): ")
				      "~/"
				      mud-macro-commands-file))))
  (setq mud-macro-commands-file filename)
  (save-excursion
    (let ((tmp (set-buffer (get-buffer-create " *MUD Macros*"))))
      (erase-buffer)
      (mud-macro-delist-all)
      (mud-macro-delist-global-triggers)
      (mud-macro-delist-type-triggers)
      (mud-macro-delist-world-triggers)
      (write-region (point-min) (point-max) filename)
      "(kill-buffer (current-buffer))")))

(defun mud-remove-macro (name)
  (interactive (list (completing-read "Macro to remove: "
				      mud-macro-commands-alist)))
  (setq mud-macro-commands-alist
	(delq (assoc name mud-macro-commands-alist) mud-macro-commands-alist)))

(defun mud-macro-command (arg)
  "Insert into stream one of the commands in mud-macro-commands-alist.
Without command argument, opens buffer for editing.  With argument,
sends alist entry directly to process."
  (interactive "P")
  (let* ((name (or (if (stringp arg) arg)
		   (completing-read "MUD Macro: "
				    mud-macro-commands-alist)))
	 (macro
	  (or (assoc name mud-macro-commands-alist)
	      (progn
		(message "Macro unknown... providing empty buffer.")
		(list name nil nil)))))
    (if arg
	(mud-send-here (mud-macro-sendable macro))
      (let* ((buf (current-buffer))
	     (proc (mud-process)))
	(pop-to-buffer (get-buffer-create (concat "*" name "*")))
	(if (or (equal (buffer-string) "")
		(y-or-n-p (concat "Erase buffer " name "? ")))
	    (progn
	      (erase-buffer)
	      (mud-macro-delist macro)
	      (goto-char (point-min))
	      (forward-line)
	      (mud-macro-expansion-mode)
	      (mud-copy-here-from buf)
	      (setq mud-expansion-macro-name name)))))))

(provide 'j-mud-macros)
