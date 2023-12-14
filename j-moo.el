;;; MOO
(require 'j-tiny)

(mud-subtype "MOO" "TinyMUD"
  '(filters . moo-filter-hook)
  '(startup-hook . moo-mode-hook)
  '(page-regexp . "\\(You sense that [^ ]* is looking for you in \\)")
  '(macro-mode . moo-macro-mode)
  '(error-string . "#[0-9]*:[^ ]*\\( (this == #[1-9]*)\\)?, line [1-9]*.*$")
  '(notify-format . ";notify(player, \"%s\")\n")
  )

(defconst moo-object-regexp "#[0-9]+")

(defvar moo-mode-hook 'moo-mode)
(defvar moo-use-@program nil "*If t, use @program instead of .program.")

(defun moo-define-keys (standard)
  (let ((map (copy-keymap standard)))
    (define-key map "\^c\^d" 'moo-get-description)
    (define-key map "\^c\^h" 'moo-get-help-text)
    (define-key map "\^c\^f" 'moo-get-property)
    (define-key map "\^c\^v" 'moo-get-verb)
    (define-key map "\^c\^u" 'moo-unquote-message)
    (define-key map "\^c\^e" 'moo-explode-list)
    (define-key map "\^c\^i" 'moo-implode-list)
    (define-key map "\^c\^k" 'moo-comment-code)
    (define-key map "\^c\^w" 'moo-whisper)
    (define-key map "\^c\^b" 'moo-get-boom-tree-node)
    (define-key map "\^c\^o" 'moo-get-@download)
    map))

(defun moo-get-from-@display ()
  (interactive)
  (beginning-of-line)
  (cond ((looking-at "[,.]\\(.+\\) +[^ ]+ (#[0-9]+) +\".*\" +.*$")
	 (let ((property (mud-match 1))
	       (object (save-excursion
			 (beginning-of-line)
			 (moo-@display-identify-object))))
	   (if object
	       (moo-get-property (concat object "." property))
	     (error "Couldn't identify object."))))
	((or (looking-at " *\\(#[0-9]*:\"[^\"]*\"\\) *[^ ]+ (#[0-9]+) +[rxd ]+ *\\( .+\\)$")
	     (looking-at " *\\(#[0-9]*:[^\" ]+\\) *[^ ]+ (#[0-9]+) +[rxd ]+ *\\( .+\\)$"))
	 (moo-get-@edit (concat (moo-strip-verb-name (mud-match 1))
				(mud-match 2))))
	((looking-at "  \\(Owned by\\|Child of\\|Location\\) .+(\\(#[0-9]+\\))\.$")
	 (moo-get-@display (mud-match 2)))
	((looking-at ".*")
	 (error "Couldn't figure out how to get: %s" (mud-match 0)))))

(defun moo-strip-verb-name (verb-name)
  (let ((space (string-match " " verb-name)))
    (moo-strip-asterisk
     (if (or (not space) (zerop space))
	 verb-name
       (substring verb-name 0 space)))))

(defun moo-strip-asterisk (verb-name)
  (let ((star (string-match "\\*" verb-name)))
    (if (or (not star) (zerop star))
	verb-name
      (concat (substring verb-name 0 star)
	      (moo-strip-asterisk (substring verb-name (1+ star)))))))
					       
(defvar moo-macro-mode-map (moo-define-keys mud-macro-expansion-mode-map)
  "Keymap used in mud-macro mode when working with a MOO.")

(defvar moo-@display-map
  (let ((map (copy-keymap moo-macro-mode-map)))
    (define-key map "\n" 'moo-get-from-@display)
    map)
  "Adds C-j to moo-macro-mode-map")

(defvar moo-@upload-obj-map
  (let ((map (copy-keymap moo-macro-mode-map)))
    (define-key map "\n" 'moo-get-from-@download)
    map)
  "Adds C-j to moo-macro-mode-map")

(defvar moo-rmail-map
  (let ((map (copy-keymap moo-macro-mode-map)))
    (define-key map "\n" 'moo-get-rmail-message)
    map)
  "Adds C-j to moo-macro-mode-map")

(defvar moo-mode-map (moo-define-keys mud-interactive-mode-map)
  "Keymap used in mud-macro mode when working with a MOO.")

(defun moo-macro-mode ()
  (use-local-map moo-macro-mode-map))

(defun moo-mode ()
  (use-local-map moo-mode-map))

(defun moo-whisper (recipient)
  (interactive "sRecipient: ")
  (insert "whisper \"\" to " recipient)
  (mud-beginning-of-line)
  (forward-char 9))

(defun moo-@paste-kill (&optional prefix)
  "Send whatever's in the kill ring to the MOO, using #8855's @paste command."
  (interactive (list (if current-prefix-arg
			 (read-string "Prefix: " "|")
		       "")))
  (mud-send-here (concat "@paste " prefix "\n" (car kill-ring) "\n.")))

(defun moo-explode-list (narrow start end)
  "Convert a list of strings into more readable/editable text."
  (interactive (cons current-prefix-arg
		     (if current-prefix-arg
			 (list (region-beginning) (region-end))
		       (list (point-min) (point-max)))))
  (save-restriction
    (if narrow (narrow-to-region start end)
      (setq mud-exploded t))
    (goto-char (point-min))
    (mud-perform-replace "\\\\"    "\1")
    (mud-perform-replace "\\\""    "\2")
    (mud-perform-replace "{\""     "{\n")
    (mud-perform-replace "\", \""  "\n")
    (mud-perform-replace  "\"}"    "\n}")
    (mud-perform-replace "\2"      "\"")
    (mud-perform-replace "\1"      "\\")
    (goto-char (point-max))
    (if (= (preceding-char) ?\n)
	(delete-char -1))))

(defun moo-implode-list (narrow start end)
  "Convert readable/editable text into a list of strings."
  (interactive (cons current-prefix-arg
		     (if current-prefix-arg
			 (list (region-beginning) (region-end))
		       (list (point-min) (point-max)))))
  (save-restriction
    (if narrow (narrow-to-region start end)
      (setq mud-exploded nil))
    (untabify (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "{")
    (forward-char -1)
    (mud-perform-replace "\""  "\1")
    (mud-perform-replace "\\"  "\2")
    (mud-perform-replace "{\n" "{\"")
    (mud-perform-replace "\n}" "\"}")
    (mud-perform-replace "\n"  "\", \"")
    (mud-perform-replace "\2"  "\\\\")
    (mud-perform-replace "\1"  "\\\"")))

(defun moo-unquote-message ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "^\"")
	  (delete-char 1))
      (forward-line 1))))

(defun moo-finish-long-result ()
  (interactive)
  (mud-send-here (concat ";notify(player,\"~end~~~~" mud-prefix-id "~\")\n")))

(defun moo-get-help-text (help-text)
  "Fetch a given help text and explode it."
  (interactive "sEdit which help text: ")
  (if (mud-make-fetch-buffer help-text t)
      (mud-do-fetch (concat "@gethelp " help-text)
		   "help-text" help-text)))

(defun moo-get-@display (object)
  "Fetch a given object (possibly with additional information) and explode it."
  (interactive (list (read-string "Display object: "
				  (save-excursion
				    (while (looking-at "\\sw")
				      (forward-char -1))
				    (if (looking-at "\\Sw*\\(\\sw*\\)")
					(mud-match 1))))))
  (if (mud-make-fetch-buffer object t)
      (mud-do-fetch (concat "@d " object ":.")
		    "@display" object)))

(defun moo-get-@download (object)
  "Fetch a given object (possibly with additional information) and explode it."
  (interactive (list (read-string "Download: "
				  (save-excursion
				    (while (looking-at "\\sw")
				      (forward-char -1))
				    (if (looking-at "\\Sw*\\(\\sw*\\)")
					(mud-match 1))))))
  (mud-send-here (concat "@download " object)))

(defun moo-get-from-@download ()
  "Fetch the object identified on this line."
  (interactive)
  (moo-get-@download
   (let ((return (point))
	 (line 
	  (buffer-substring
	   (progn
	     (back-to-indentation)
	     (point))
	   (progn
	     (end-of-line)
	     (point)))))
     (goto-char return)
     line)))

(defun moo-fix-@display ()
  (mud-fix-unknown)
  (toggle-read-only)
  (use-local-map moo-@display-map))

(defun moo-fix-@upload-obj ()
  (if (looking-at "^@upload-obj #\\([0-9]+\\)$")
      (moo-object-has-buffer (moo-make-object mud-here
					      (string-to-int (mud-match 1)))
			     (current-buffer)))
  (mud-fix-unknown)
  (toggle-read-only)
  (use-local-map moo-@upload-obj-map))

(defun moo-fix-@upload-verb ()
  (moo-code-mode)
  (mud-copy-here-from buff))

(defun moo-get-property (property)
  "Fetch the value of some property."
  (interactive "sEdit what property: ")
  (if (mud-make-fetch-buffer property t)
      (mud-do-fetch (concat "@show " property) "property" property)))

(defun moo-get-description (desc)
  "Fetch the description of some object."
  (interactive "sEdit description of what object: ")
  (moo-get-property (concat desc ".description")))

(defun moo-rmail (collection)
  "Fetch your mail summary."
  (interactive (list
		(read-from-minibuffer "Mail from what collection: "
				      (save-excursion
					(beginning-of-line)
					(if (looking-at "    \\(\\S-*\\)$")
					    (mud-match 1)
					  "")))))
  (if (or (equal collection "") (equal collection "me"))
      (setq collection "mail")
    (setq collection (concat "mail on " collection)))
  (if (mud-make-fetch-buffer (concat "MOO-R" collection) t)
      (mud-do-fetch (concat "@" collection) "rmail"
		   (concat "MOO-R" collection))))

(defun moo-fix-rmail ()
  (mud-fix-unknown)
  (use-local-map moo-rmail-map))

(defun moo-@display-identify-object ()
  (cond ((or (looking-at "[,.]\\(.+\\) +[^ ]+ (#[0-9]+) +\".*\" +.*$")
	     (looking-at " *\\(#[0-9]*:\\)\"\\([^ ]*\\)\\( [^\" ]+\\)*\" *[^ ]+ (#[0-9]+) +\".+\" +.+$")
	     (looking-at " *\\(#[0-9]*:\\)\\([^\" ]+\\) *[^ ]+ (#[0-9]+) +\".+\" +.+$")
	     (looking-at "  \\(Owned by\\|Child of\\|Location\\) .+(\\(#[0-9]+\\))\.$"))
	 (cond ((eq (forward-line -1) -1)
		(cond ((string-match moo-object-regexp (buffer-name))
		      (buffer-name))))
	       (t
		(moo-@display-identify-object))))
	((looking-at ".* (\\(#[0-9]+\\)) \\[.*\\]")
	 (mud-match 1))
	(t
	 nil)))

(defun moo-rmail-identify-folder ()
  (cond ((looking-at ">? *\\([0-9]+\\):  .*$")
	 (cond ((eq (forward-line -1) -1)
		(cond ((string-match "MOO-Rmail on \\(\\S-+\\)"
				     (buffer-name))
		       (mud-match-string 1 (buffer-name)))))
	       (t
		(moo-rmail-identify-folder))))
	((looking-at "^[0-9]+ messages? on \\(\\S-+\\) (#[0-9]+):$")
	 (mud-match 1))
	((looking-at ">?@mail.* on \\(\\S-+\\).*$")
	 (mud-match 1))
	(t
	 nil)))
	 
(defun moo-get-rmail-message ()
  (interactive)
  (let* ((folder 
	  (save-excursion
	    (beginning-of-line)
	    (moo-rmail-identify-folder)))
	 (messagenum
	  (save-excursion
	    (beginning-of-line)
	    (cond ((looking-at ">? *\\([0-9]+\\):  .*")
		   (mud-match 1)))))
	 (message (cond (messagenum
			 (concat messagenum (if folder
						(concat " on " folder))))
			(t
			 (error "Couldn't figure out what message you wanted.")))))
    (if (mud-make-fetch-buffer message t)
	(mud-do-fetch (concat "@read " message) "rmail-message"
			     message))))

(defun moo-fix-property ()
  (mud-fix-unknown)
  (insert "; !(")
  (search-forward ".")
  (insert "(\"")
  (end-of-line)
  (insert "\") = ")
  (let ((start (point)))
    (re-search-forward "Value: *")
    (delete-region start (point)))
  (save-excursion
    (end-of-line)
    (insert ")")))

(defun moo-get-@edit (verb)
  "Edit the MOO code for a particular verb."
  (interactive "sProgram what verb: ")
  (mud-send-here (concat "@edit " verb)))

(defun moo-fix-@program ()
  (moo-code-mode)
  (mud-copy-here-from buff))

(defun moo-get-verb (verb)
  "Fetch the MOO code for a particular verb."
  (interactive "sProgram what verb: ")
  (mud-send-here (concat "@edit " verb)))

(defun moo-get-@list (verb)
  "Fetch the MOO code for a particular verb."
  (interactive "sProgram what verb: ")
  (if (mud-make-fetch-buffer verb t)
      (mud-do-fetch (concat "@list " verb " without numbers")
		   "@list" verb)))

(defun moo-fix-@list ()
  (moo-code-mode)
  (mud-copy-here-from buff)
  (cond ((looking-at "That object")
	 (let ((message (substring (buffer-string) 0 -1)))
	   (erase-buffer)
	   (error message)))
	((looking-at "That verb")
	 (let ((start (point)))
	   (end-of-line)
	   (delete-region start (point)))))
  (let* ((objnum (and (looking-at "Object \\(#[0-9]+\\) does not define that verb, but its ancestor \\(#[0-9]+\\) does.") (mud-match 1)))
	 (ancestor (and objnum (mud-match 2)))
	 (vname (buffer-name))
	 args)
    (if objnum
	(delete-region (point) (save-excursion (forward-line 1) (point))))
    (if (looking-at
	 "\\(#[0-9]+\\):\\([^\"][^ ]*\\|\"[^\"]+\"\\)   \\([a-z]+ .+ [a-z]+\\)$")
	(progn
	  (setq vname (concat (or objnum (mud-match 1)) ":" (mud-match 2))
		args (mud-match 3))
	  (if (string-match "\\(.+ \\)(.+/\\(.+\\))\\( .+\\)" args)
	      (setq args (concat (mud-match-string 1 args)
				 "\"" (mud-match-string 2 args) "\""
				 (mud-match-string 3 args))))
	  (delete-region (point) (save-excursion (forward-line 1) (point)))
	  (insert "@args " vname " " args "\n")))
    (if (not (re-search-forward "^@program" nil t))
	(insert (concat (if moo-use-@program
			    "@program "
			  ".program ") (buffer-name) "\n")))
  (if ancestor (insert "\"Definition from " ancestor "\";\n")))
;  (if (looking-at "#")				; Kill the header line.
;      (let ((start (point)))
;	(beginning-of-line 2)
;	(delete-region start (point))))
  (goto-char (point-max))
  (insert ".\n")
  (goto-char (point-min))
  (beginning-of-line 2))

(defvar moo-filter-hook
  '(mud-check-reconnect mud-fill-lines))


(defvar moo-object-buffers '()
  "Alist of objects we have info about in buffers.")
(defvar moo-verb-buffers '()
  "Alist of verbs we have code for in buffers.")
(defvar moo-property-buffers '()
  "Alist of properties we have values for in buffers.")

(defun moo-make-object (world objnum)
  "Make a `moo object' referring to OBJNUM (a number) on WORLD (a symbol in mud-worlds."
  (cons world objnum))
(defun moo-object-world (object)
  "What world does OBJECT belong to?"
  (car object))
(defun moo-object-number (object)
  "What number is OBJECT on its world?"
  (cdr object))

(defun moo-make-verb (object names args)
  "Make a `moo verb' referring to OBJECT's verb with given NAMES and ARGS.
OBJECT is a `moo object' (see moo-make-object).
NAMES is a list of strings.
ARGS is another list of strings."
  (cons object (cons names args)))
(defun moo-verb-object (verb)
  "What object is VERB defined on?
VERB is a `moo verb' (see moo-make-verb).
Returns a `moo object' (see moo-make-object)."
  (car verb))
(defun moo-verb-names (verb)
  "What are the names of VERB?
VERB is a `moo verb'.
Returns a list of strings."
  (car (cdr verb)))
(defun moo-verb-args (verb)
  "What are the args for VERB?
VERB is a `moo verb'.
Returns a list of strings."
  (cdr (cdr verb)))

(defun moo-object-has-buffer (object buffer)
  "Mark that we have info about OBJECT (a moo-object) stored in BUFFER.
Return the buffer formerly associated with OBJECT, or nil if we have no prior knowledge of it."
  (moo-have-something object buffer 'moo-object-buffers))

(defun moo-object-buffer (object)
  "What buffer has info about OBJECT?"
  (moo-which-buffer object moo-object-buffers))

(defun moo-verb-has-buffer (verb buffer)
  "Mark that we have info about VERB (a moo-verb) stored in BUFFER.
Return the buffer formerly associated with VERB, or nil if we have no prior knowledge of it."
  (moo-have-something verb buffer 'moo-verb-buffers))

(defun moo-verb-buffer (verb)
  "What buffer has info about VERB?"
  (moo-which-buffer verb moo-verb-buffers))

(defun moo-property-has-buffer (property buffer)
  "Mark that we have info about PROPERTY (a moo-property) stored in BUFFER.
Return the buffer formerly associated with PROPERTY, or nil if we have no prior knowledge of it."
  (moo-have-something property buffer moo-property-buffers))

(defun moo-property-buffer (property)
  "What buffer has info about PROPERTY?"
  (moo-which-buffer property 'moo-property-buffers))

(defun moo-have-something (something buffer alist)
  "Mark that we have info about SOMETHING stored in BUFFER, using the alist denoted by the symbol ALIST.
Return the buffer formerly associated with SOMETHING, or nil if we have no prior knowledge of it."
  (mud-buffer-has buffer something)
  (mud-mark-association something buffer alist))

(defun moo-unhave-something (buffer alist)
  "Mark that we no longer have info about SOMETHING stored in BUFFER, using the alist denoted by the symbol ALIST."
  (mud-remove-association (mud-info-for-buffer buffer) alist)
  (mud-buffer-unhas buffer))

(defun moo-which-buffer (something alist)
  "Find the buffer that contains info about SOMETHING, using ALIST.
ALIST should be pairs of (<something-type> . <buffer>)."
  (let ((cur (assoc something alist)))
    (if cur
	(cdr cur)
      nil)))

(provide 'j-moo)
