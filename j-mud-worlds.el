;;; Reading from world file
;;;
(defvar mud-world-file "~/.jmud_worlds"
  "*Pathname to location of MUD address/character/password file.
For more info, see function mud-load-worlds.")

(defvar mud-max-worlds 200 "Maximum number of MUD worlds that can be defined.")

(defvar mud-worlds (make-vector mud-max-worlds 0)
  "Obarray of all defined worlds.
Size is determined at load time by mud-max-worlds.")

(defvar mud-default-world "" "*Default server name.")

(defun mud-copy-here-from (buffer)
  (setq mud-here
	(save-excursion
	  (set-buffer buffer)
	  mud-here))
  (make-local-variable 'mode-line-process)
  (let ((s (concat "@" (symbol-name mud-here))))
    (setq mode-line-process
	  (if (> (length s) 20) (substring s 0 20) s))))

(defvar mud-world-file-dates nil)

(defun mud-file-write-date (file)
  (nth 5 (file-attributes file)))

(defun mud-load-worlds (name)
  (interactive "fWorld-file to read from: ")
  (let ((file (expand-file-name name))
	(old-buffer (current-buffer))
	(buffer (generate-new-buffer " *MUD temp*")))
    (if (file-exists-p file)
	(progn	(setq mud-world-file-dates (cons (cons file (mud-file-write-date file))
						 mud-world-file-dates))
		(load-file file))
      (mud-add-world "LambdaMOO" (if (mud-world "MOO")
				     "MOO"
				   "global")
		     nil nil "lambda.parc.xerox.com" 8888 nil)
      (message "Can't find MUD world-file %s; added world LambdaMOO" file))))

(defun mud-check-world-file ()
  (if (or (null mud-world-file-dates)
	  (let ((dates mud-world-file-dates))
	    (while (and dates
			(equal (cdr (car dates))
			       (mud-file-write-date (car (car dates)))))
	      (setq dates (cdr dates)))
	    (not (null dates))))
      (progn
	(setq mud-world-file-dates nil)
	(if (file-exists-p mud-world-file)
	    (mud-load-worlds mud-world-file)))))

;;; creating new worlds and world types
(defun mud-read-pairs ()
  (let ((pair-list nil) pair)
    (while (setq pair
		 (read-from-minibuffer "(Property . value) or nil to end input: "
				       nil nil t))
      (setq pair-list (cons pair pair-list)))
    pair-list))

(defun mud-subtype (world-name parent-name &rest pairs)
  "Create a child of PARENT-NAME named WORLD-NAME; the rest of the arguments should be pairs of the form (property . value)."
  (interactive (list
		(read-string "World name: ")
		(mud-name
		 (completing-read "Parent name: " mud-worlds nil t))
		(mud-read-pairs)))
  (let ((parent (mud-world parent-name))
	(world (or (mud-world world-name)
		   (mud-new-world world-name))))
    (if parent
	(progn
	  (mud-world-change-parent world parent)
	  (mud-world-set-pairs world pairs))
      (error "MUD parent unknown: %s" parent-name))
    world))

(defun mud-blank-to-nil (string)
  (if (equal string "")
      nil
    string))

(defun mud-nil-to-blank (string)
  (or string ""))

(defun mud-add-world (world-name parent-name login password site port
				 macro-file &rest pairs)
  "Add world named WORLD, parent PARENT-NAME, with LOGIN, PASSWORD, SITE, PORT, MACRO-FILE, and rest PAIRS of form (property . value)"
  (interactive (list
		(read-string "World name: ")
		(completing-read "Parent name: " mud-worlds nil t)
		(read-string "Login: ")
		(read-string "Password: ")
		(read-string "Site: ")
		(string-to-int (read-string "Port number: "))
		(read-string "Macro file: ")))
  (apply 'mud-subtype (append
		       (list
			world-name parent-name
			(mud-blank-to-nil (mud-pair login))
			(mud-blank-to-nil (mud-pair password))
			(mud-pair site)
			(mud-pair port)
			(mud-blank-to-nil (mud-pair macro-file)))
		       pairs)))

(defun mud-request-world-name (&optional predicate)
  "Ask user for name of world to connect to."
  (mud-check-world-file)
  (intern
   (completing-read "Server: "
		    mud-worlds
		    predicate t
		    mud-default-world)
   mud-worlds))

;;; standard accessor functions associated with mud worlds and types
(defun mud-fixer (mud name)
  (let ((parent (mud-parent mud)))
    (or (intern-soft (concat (downcase (mud-name mud)) "-fix-" name))
	(if parent
	    (mud-fixer parent name)
	  'mud-fix-unknown))))

(defun mud-self-gag-list (&optional mud)
  (mud-world-get-splice (or mud mud-here) 'self-gag-list))
(defun mud-self-gagp ()
  (let ((gaglist (mud-self-gag-list)) gagp)
    (while (and gaglist 
		(not (setq gagp
			   (cond ((looking-at (car gaglist))
				  t)))))
      (setq gaglist (cdr gaglist)))
    gagp))

(defun mud-login (mud)
  (mud-world-get mud 'login))
(defun mud-password (mud)
  (mud-world-get mud 'password))
(defun mud-connect-command (mud)
  (mud-world-get mud 'connect-command))
(defun mud-connect-filter (mud)
  (mud-world-get mud 'connect-filter))
(defun mud-name (mud)
  (symbol-name mud))
(defun mud-site (mud)
  (mud-world-get mud 'site))
(defun mud-port (mud)
  (mud-world-get mud 'port))
(defun mud-page-regexp (mud)
  (mud-world-get mud 'page-regexp))
(defun mud-pending-output (mud)
  (get mud 'pending-output))
(defun mud-process (&optional mud)
  (get (or mud mud-here) 'process))
(defun mud-trigger-list (&optional mud)
  (apply #'append (mud-world-get-all (or mud mud-here) 'trigger-list)))
(defun mud-output-buffer (&optional mud)
  (mud-world-get (or mud mud-here) 'output-buffer))
(defun mud-prompt (&optional mud)
  (mud-world-get (or mud mud-here) 'prompt))
(defun mud-filters (&optional mud)
  (mud-world-get (or mud mud-here) 'filters))
(defun mud-startup-hook (&optional mud)
  (mud-world-get (or mud mud-here) 'startup-hook))
(defun mud-macro-mode (&optional mud)
  (mud-world-get (or mud mud-here) 'macro-mode))
(defun mud-fill-input (&optional mud)
  (mud-world-get (or mud mud-here) 'fill-input))
;;; world utilities: the backbone of the world/type system
(defun mud-world (name)
  (intern-soft name mud-worlds))

(defun mud-new-world (name)
  (intern name mud-worlds))

(defun mud-parent (world)
  (get world 'parent))

(defun mud-world-change-parent (world parent)
  (mud-world-set-pair world (mud-pair parent)))

(defmacro mud-pair (symbol)
  (list 'cons (list 'quote symbol) symbol))

(defun mud-world-set-pair (world pair)
  (put world (car pair) (cdr pair)))

(defun mud-world-set-pairs (world pairs)
  (while pairs
    (mud-world-set-pair world (car pairs))
    (setq pairs (cdr pairs))))

(defun mud-world-get-string (mud property)
  (mud-nil-to-blank (mud-world-get mud property)))

(defun mud-world-get (mud property)
  (let ((value (get mud property))
	parent)
    (or value
	(if (setq parent (mud-parent mud))
	    (mud-world-get parent property)))))

(defun mud-world-get-all (mud property)
  (let ((parent (mud-parent mud)))
    (cons (get mud property)
	    (if parent
		(mud-world-get-all parent property)))))

(defun mud-world-get-splice (mud property)
  (let ((value (get mud property))
	(parent (mud-parent mud)))
    (if parent
	(let ((pass (mud-world-get-splice parent property)))
	  (if value
	      (mud-splice value 'pass pass)
	    pass))
      value)))
      
(defun mud-splice (first where what)
  (cond ((eq (car first) where)
	 (append what (cdr first)))
	((car first)
	 (cons (car first) (mud-splice (cdr first) where what)))))

;;; initialize root world
(mud-world-set-pairs (mud-new-world "global")
		     '((prompt . "")
		       (connect-command . "")
		       (start-process . (lambda (name buf site port) ;strings
					  (if (string-match "^[.0-9]+$" site)
					      (start-process name buf "telnet"
							     site
							     (int-to-string
							      port))
					    (open-network-stream name buf
								 site
								 port))))))

(provide 'j-mud-worlds)
