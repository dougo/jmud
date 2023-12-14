;;; Major Mode for talking to MUDs
;;; by James Aspnes (asp@cs.cmu.edu) and Stewart Clamen (clamen@cs.cmu.edu)
;;; and Pavel Curtis (pavel@parc.xerox.com)
;;; 1989, 1990, 1991
;;;
;;; MODIFICATION HISTORY
;;; 
;;; May/June 1990 - Generalized to handle SMUG and LPMUD
;;; 
;;; January  1991 - Added Pavel Curtis' MOO support and assorted bug fixes, 
;;;                 also changed process-status call to run under 18.56.
;;;
;;; February 1991 - Added input-history browsing support (M-p/M-n);
;;;                   commands for sending the latest item on the kill
;;;                   ring (C-c C-y), optionally with each line bracketed by a
;;;                   given prefix and suffix (C-u C-c C-y); and a command to
;;;                   clear the current input line (C-c C-u).
;;;                 Added support for adding/overriding server definitions
;;;                   and the default server in one's .mud file.
;;;                 Fixed some bugs.
;;;                 Added support for people who prefer that the type-in
;;;                   point for a MUD buffer stay glued to the bottom of the
;;;                   window (see the 'mud-use-entire-window' option).
;;;
;;; May 1991 - Added mud-mode command.  This is not the official released
;;; version.  I put mine on belch cuz the other one got deleted. --yduJ
;;;
;;; Current  1991 - Recognizes tinytalk-style /addworld commands.
;;;                 More moo support: automatic quoting of files, more
;;;                 use of Pavel's text capture.  This stuff might work on
;;;                 otherMUDs too; I wouldn't know.  --JoeFeedback
;;;                                                  (94eco@cc.williams.edu)
;;;
;;;                 Lots of strange revisions I don't want to describe
;;;                   right now; prep for a MOO browser.
;;;
;;; Oct 1991 - worlds are in an obarray, instead of a list.  filter function
;;;            no longer inserts incomplete lines.  

(require 'j-mud-worlds)
(require 'j-mud-get)
(require 'j-mud-macros)
(require 'j-mud-history)

(defvar mud-do-activity-alert nil "*If true, put an alert in the mode line when there's activity in an unshown buffer.")

(defconst mud-last-active "" "Name of last mud that was active")

(if mud-do-activity-alert
    (setq global-mode-string
	  (append (or global-mode-string '("")) '(mud-last-active))))

(defun mud-activity-alert ()
  "Tell the player that something's going on, if this mud isn't being displayed."
  (cond ((not (get-buffer-window (process-buffer (mud-process mud-here))))
	 (message "%s: %s"
		  (mud-name mud-here)
		  (buffer-substring (point-min)
				    (max (point-min) (1- (point)))))
	 (setq mud-last-active (concat " [" (mud-name mud-here) "]")))
	((equal mud-last-active (concat " [" (mud-name mud-here) "]"))
	 (setq mud-last-active nil))))
(defun mud-clear-activity-alert ()
  "Okay, okay, I know something's going on already."
  (interactive)
  (setq mud-last-active nil))

(defvar mud-here nil "*Symbol for the current mud.")
(make-variable-buffer-local 'mud-here)

(defvar mud-accept-reconnects 50
  "*If nil, reject reconnect signals. If non-nil, accept reconnect signals 
by breaking existing connection and establishing new connection.  If an
integer, spawn <n> connections before breaking any.")

(defvar mud-use-entire-window nil
  "*Try to keep the type-in point for a MUD buffer at the bottom of the window.")

(defvar mud-use-suppress-all-input nil "*If t, use mud-suppress-all-input for MUDs with non-symbolic addresses.")

(defvar mud-fill-prefix "    " "*String to place before each line but the first in each filled paragraph.")

(defmacro mud-match (n)
  (list 'if (list 'match-beginning n)
	(list 'buffer-substring
	      (list 'match-beginning n) (list 'match-end n))))

(defmacro mud-match-string (n str)
  (list 'substring str (list 'match-beginning n) (list 'match-end n)))

(defvar mud-reconnect-regexp
  "#### Please reconnect to \\([^@]*\\)@\\([^ @]*\\) *\\(\\|([^ @]*)\\) port \\([0-9]+\\) ####.*$"
  "Regular expression for detecting reconnect signals.")

(defconst mud-new-connectionp nil
  "Flag to identify hail for new connection")

(defun mud-check-reconnect ()
  "Look for reconnect signal and open new connection if none to that
site already exists."
  (goto-char (point-min))
  (while (not (eobp))
    (if (and mud-accept-reconnects (looking-at mud-reconnect-regexp))
	(let ((name (mud-match 1))
	      (site-addr (mud-match 2))
	      (site (and (not (eq (match-beginning 3)
				  (match-end 3)))
			 (buffer-substring (1+ (match-beginning 3))
					   (1- (match-end 3)))))
	      (port (string-to-int (mud-match 4))))
	  (delete-region (match-beginning 0) (match-end 0))
;;; Which is site and which site-addr?
	  (let* ((world (or (mud-world name)
			    (mud-add-world name "global"
					   nil nil site-addr port
					   nil))))
	    (cond
	     ((zerop port)
	      (message "Illformed portal signal. Inform Builder."))
	     (t
	      (save-excursion
		(setq mud-new-connectionp (concat "*" name "*"))
		(mud world t)))))))
    (beginning-of-line 2)))

(defun mud-fill-lines ()
  "Fill buffer line by line."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((break (move-to-column (1+ fill-column))))
      (if (<= break fill-column)
	  (beginning-of-line 2)
	;; else fill
	(skip-chars-backward "^ \n")
	(skip-chars-backward " ")
	(if (bolp)
	    ;; can't fill, we lose
	    (beginning-of-line 2)
	  (delete-horizontal-space)
	  (insert ?\n fill-prefix))))))

(defmacro mud-perform-replace (from to)
  "Replace one string with another."
  (list 'save-excursion
	(list 'while (list 'search-forward from nil t)
	      (cond ((not (equal to ""))
		     (list 'replace-match to t t))
		    (t
		     (list 'delete-char
			   (if (stringp from)
			       (- (length from))
			     (list '- (list 'length from)))))))))

(defun mud-redirect-lines (lines)
  "Can't use mapcar because redirect function may change at an awkward time."
  (while lines
    (apply (mud-world-get mud-here 'redirect-function) (list (car lines)))
    (setq lines (cdr lines))))

(defun mud-filter (proc string)
  "Filter for input from MUD process.  Calls MUD-specific filters as well. 
Also, if recently established new connection automatically, check to see 
if number of active connections exceeded connection limit and delete 
current process if so."
  (let ((mud-goto-buffer nil))
    (save-excursion
      (set-buffer (process-buffer proc))
      (setq mud-output
	    (mud-string-to-list (concat (mud-pending-output mud-here) string)))
      (put mud-here 'pending-output (car mud-output))
      (setq mud-output (cdr mud-output))
      (if mud-output
	  (let (start end)
	    (goto-char (marker-position (process-mark proc)))
	    (setq start (point))
	    (mud-redirect-lines mud-output)
;	    (mapcar (mud-world-get mud-here 'redirect-function) mud-output)
;	    (apply (mud-world-get mud-here 'redirect-function) mud-output nil)
	    (save-restriction
	      (narrow-to-region start (point))
	      (goto-char start)
	      (run-hooks (mud-filters))))))
    (mud-recenter proc)
    (mud-close-extra-connections)
    (mud-display-buffer mud-goto-buffer)))

(defun mud-display-buffer (buffer)
  (if buffer
      (display-buffer buffer)))

(defun mud-string-to-list (string)
  (let ((list nil))
    (while (string-match "\^m?\n" string)
      (setq list (cons (substring string 0
				  (match-beginning 0))
		       list))
      (setq string (substring string (match-end 0) (length string))))
    (cons string (reverse list))))
  
(defun mud-recenter (proc)
  "If we should recenter, recenter."
  (if (and (eq (current-buffer) (process-buffer proc))
	   (eq scroll-step 1)
	   (= (point) (point-max)))
      (recenter -1)))

(defun mud-close-extra-connections ()
  "See if we have too many connections open."
  (if mud-new-connectionp
      (progn
	(if (or			     ; Do we close current connection?
	     (not (numberp mud-accept-reconnects))
	     (let ((c mud-accept-reconnects) (l (process-list)))
	       (while l
		 (if (and (eq (process-filter (car l)) 'mud-filter)
			  (memq (process-status (car l)) '(open run)))
		     (setq c (1- c)))
		 (setq l (cdr l)))
	       (< c 0)))
	    (progn
	      (delete-process (get-buffer-process (current-buffer)))
	      (kill-buffer (current-buffer))))
	(pop-to-buffer mud-new-connectionp)
	(if (> baud-rate search-slow-speed) (recenter))
	(setq mud-new-connectionp nil))))
	    
(defun mud-eobp ()
  (cond ((eobp)
	 t)
	((looking-at ".*\\S-")
	 nil)
	(t
	 (forward-line)
	 (mud-eobp))))

(defun mud-send ()
  "Send current line of input to a MUD (mud-here)."
  (interactive)
  (let ((proc (mud-process mud-here)))
    (cond ((and proc (memq (process-status proc) '(open run)))
	   ;; process exists, send line
	   (let* ((origin (point))
		  (end (mud-find-input))
		  (line (buffer-substring (point) end)))
	     (mud-send-here line)
	     (mud-remember-input line)
	     (cond ((save-excursion
		      (end-of-line)
		      (mud-eobp))
		    (cond ((mud-self-gagp)
			   (beginning-of-line)
			   (delete-region (point) end)
			   (delete-backward-char 1))
			  ((mud-fill-input)
			   (narrow-to-region (point) end)
			   (mud-fill-lines)
			   (widen)))
		    (goto-char (point-max))
		    (insert ?\n)
		    (move-marker (process-mark proc) (point))
		    (insert (mud-prompt))
		    (if (= scroll-step 1)
			(recenter -1)))
		   (t
		    (message "Sent line \"%s\"" line)
		    (goto-char origin)))))
	  (t
	   (message "Not connected--- nothing sent.")
	   (insert ?\n)))))

(defun mud-find-input ()
  "Move point to mud-beginning-of-line, and return end-of-line."
  (end-of-line 1)
  (prog1
      (point)
    (mud-beginning-of-line)))

(defun mud-beginning-of-line ()
  "Move point to beginning-of-line, but after prompt character."
  (interactive)
  (beginning-of-line 1)
  (if (looking-at (mud-prompt))
      (forward-char (length (mud-prompt)))))

(defun mud-send-kill (arg)
  "Send whatever's in the kill ring to the MOO.
With prefix, prompt for strings to place before and after each line."
  (interactive "P")
  (if arg
      (call-interactively 'mud-send-kill-prefix)
    (mud-send-here (car kill-ring))))

(defun mud-send-kill-prefix (prefix suffix)
  (interactive "sPrefix: \nsSuffix: ")
  (let ((buf (current-buffer))
	(temp (generate-new-buffer " *MUD temp*")))
    (save-excursion
      (set-buffer temp)
      (mud-copy-here-from buf)
      (yank)
      (let ((case-replace nil))
	(surround-region "" prefix suffix "" (point-min) (point-max))
	(mud-send-here (buffer-substring (point-min) (point-max)))
	(kill-buffer temp)))))

(defun mud-quit ()
  "Quit MUD process."
  (interactive)
  (if (yes-or-no-p "Are you sure you want to quit this MUD session? ")
      (progn
	(delete-process (get-buffer-process (current-buffer)))
	(put mud-here 'process nil))))

(defvar mud-mode-syntax-table nil
  "Syntax table used while in MUD interactive mode.")

(defun mud-mouse-paste (event) 
  "Spam the MUD with the current X selection, or the X cutbuffer." 
  (interactive "e") 
  (let ((text (if (eq system-type 'windows-nt)
		  (win32-get-clipboard-data)
		(or (condition-case () (x-get-selection) (error ())) 
		    (x-get-cutbuffer) 
		    (error "No selection or cut buffer available")))))
    (and (not mouse-yank-at-point) 
         (mouse-set-point event)) 
    (mud-send-here text))) 
 
(defvar mud-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'mud-send)
    (define-key map "\^a" 'mud-beginning-of-line)
    (define-key map "\^i" 'dabbrev-expand)
    (define-key map "\^c\^q" 'mud-quit)
    (define-key map "\^c\^m" 'mud-macro-command)
    (define-key map "\^c\^u" 'mud-cancel-input)
    (define-key map "\^c\^y" 'mud-send-kill)
    (define-key map "\^c\^r" 'mud-retarget)
    (define-key map "\^c\^t" 'mud-get-text)
    (define-key map "\^c\^l" 'mud-get-long-result)
    (define-key map "\ep" 'mud-previous-command)
    (define-key map "\en" 'mud-next-command)
    (define-key map (if (string-match "XEmacs\\|Lucid" emacs-version) 
                        [(shift button2)] 
                       [S-mouse-2]) 'mud-mouse-paste) 
    map)
  "Keymap for MUD interactive mode.")

(defun mud-interactive-mode (world proc)
  "Major Mode for talking to inferior MUD processes.

Commands: 
\\{mud-interactive-mode-map}

Buffer-local variables:

 mud-here
    Symbol for MUD that text is sent to.

Global Variables: [default in brackets]

 mud-fill-input                                 [t]
    If non-nil, fill all input lines.

 mud-accept-reconnects				[nil]
    If nil, reject reconnect signals. If non-nil, accept reconnect
    signals by breaking existing connection and establishing new
    connection.  If an integer, spawn that many connections before
    breaking any.

 mud-macro-commands-file                        [\"~/.jmud_macros\"]
    Pathname to location of MUD macro file.
    For more info, see help on this variable.
 mud-world-file					[\"~/.jmud_worlds\"]
    Pathname to location of MUD address/character/password file.
    For more info, see help on this variable.
 mud-use-entire-window				[nil]
    Try to keep the type-in point for the MUD buffer at the bottom
    of the window, so as not to have a half-window of blank space.

defmud-type parameters:

 prompt
    Character to identify MUD command input.
 connect-filters
    Initial filter hooks (before login)
 filters
    List of hooks to call before displaying output from MUD
    process to MUD buffer.  [Default hooks support line-filling,
    page checking, and reconnect detection.]
 startup-hook
    Hook to run at startup.  Users wishing to use macros may want to
    bind it to the following in their .emacs file:

     (setq mud-tinymud-mode-hook
           '(lambda ()
       	       (mud-load-macro-commands tinymud-macro-commands-file)))

"
  (interactive (let ((w (mud-request-world-name)))
		 (list w (or (mud-process w)
			     (get-buffer-process
			      (buffer-name (current-buffer)))))))
  (if (null proc)
      (error "No process"))
  (put world 'process proc)
  (put world 'output-buffer (current-buffer))
  (kill-all-local-variables)
  (setq mud-here world)
  (set-process-filter proc 'mud-filter)
  (setq mode-name (mud-name (mud-parent mud-here)))
  (setq major-mode 'mud-interactive-mode)
  (setq fill-column (1- (window-width)))
  (if (and mud-use-suppress-all-input
	   (string-match "[0-9.]*" (mud-site mud-here)))
      (put mud-here 'self-gag-list (cons "" (mud-self-gag-list))))
  (if (null mud-mode-syntax-table)
      (progn
	(setq mud-mode-syntax-table (make-syntax-table))
	(set-syntax-table mud-mode-syntax-table)
	(modify-syntax-entry ?\[ "(]")
	(modify-syntax-entry ?\] ")[")
	(modify-syntax-entry ?# "w"))
    (set-syntax-table mud-mode-syntax-table))
  (use-local-map (copy-keymap mud-interactive-mode-map))
  (make-local-variable 'mode-line-process)
  (let* ((s (concat "@" (symbol-name mud-here)))
	 (ss (cond ((not mud-accept-reconnects) "")
		   ((> (length s) 20) (substring s 0 20))
		   (t s))))
    (setq mode-line-process (list (concat ss ":%s"))))
  (newline)
  (goto-char (point-max))
  (set-marker (process-mark proc) (point))
  (insert (mud-prompt))
  (cond (mud-use-entire-window
	 (make-local-variable 'scroll-step)
	 (setq scroll-step 1))
	(t
	 (recenter '(4))))
  (mud-initialize-input-history)
  (setq fill-prefix mud-fill-prefix)
  (run-hooks (mud-startup-hook)))

(defun mud-send-string (string proc)
  "Send STRING as input to PROC"
  (process-send-string proc (concat string "\n")))

(defun mud-send-here (string)
  "Send STRING as input to mud-current-process."
  (process-send-string (mud-process mud-here) (concat string "\n")))

(defun mud (world autoconnect)
  "Connect to a MUD, asking for site to connect to.
With optional argument, autoconnect with login and password."
  (interactive (list
		(mud-request-world-name (if current-prefix-arg
					    'mud-login
					  'mud-site))
		current-prefix-arg))
  (setq world (intern (symbol-name world) mud-worlds))
  (mud-cleanup-extra-processes)
  (let ((buf-name (concat "*" (symbol-name world) "*")))
    (if (memq (get-buffer-process buf-name) (process-list))
	(switch-to-buffer buf-name)
      (let* ((buf (get-buffer-create buf-name))
	     (site (mud-site world))
	     (port (mud-port world))
	     (name (mud-name world))
	     (password (or (not autoconnect)
			   (mud-password world)
			   (comint-read-noecho
			    (concat "Password for " name ": ") t)))
;;			   (mud-read-password
;;			    (concat "Password for " name ": "))))
	     (proc (funcall (mud-world-get world 'start-process)
			    name buf site port)))
	(set-process-query-on-exit-flag proc t)
	(if autoconnect
	    (let ((filter (or (mud-connect-filter world)
			      'mud-filter)))
	      (set-process-filter proc filter)
	      (mud-send-string (format (mud-connect-command world)
				       (mud-login world)
				       password) proc)))
	(switch-to-buffer buf)
	(mud-interactive-mode world proc)))))

(defun mud-read-password (prompt &optional default)
  ;;; this function obsoleted by comint-read-noecho (from which it
  ;;; must in some form have been cribbed) in emacs 19.
  "Read a password from the user. Echos a . for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
^G quits.  ^Q or \\ quotes the following character (or octal digits).
Optional DEFAULT is password to start with."
  (let ((pass (or default ""))
	(echo-keystrokes 0)
	(cursor-in-echo-area t)
	inhibit-quit qcount qcode c)
    (while (progn
	     (message
	      (concat prompt
		      (make-string (length pass) ?.)
		      (if qcount (concat "\\" (make-string qcount ?#)) "")))
	     (setq c (read-char))
	     (or qcount (not (memq c '(?\r ?\n ?\e)))))
      (if inhibit-quit (setq quit-flag nil))
      (cond (qcount
	     (if (and (<= ?0 c) (<= c ?7))
		 (setq qcode (+ (- c ?0) (* qcode 8)) qcount (1+ qcount))
	       (if (eq qcount 0)
		   (setq qcode c)
		 (setq unread-command-events (cons c unread-command-events)))
	       (setq qcount 3))
	     (if (eq qcount 3)
		 (setq pass (concat pass (char-to-string qcode))
		       qcount)))
	    ((= c ?\C-u)
	     (if (string= pass "") (ding) (setq pass "")))
	    ((memq c '(?\b ?\C-?))
	     (if (string= pass "") (ding) (setq pass (substring pass 0 -1))))
	    ((memq c '(?\C-q ?\\))
	     (setq qcount 0 qcode 0))
	    (t 
	     (setq pass (concat pass (char-to-string c)))))
      (setq inhibit-quit (eq qcount 0)))
    (message "")
    pass))

;;*yduJ* Have to do some hairy hack with buffer names as above to ensure that
;;we use the right buffer name both with and without the stars, depending on
;;what the various functions here need.  Ugh.
(defun mud-shell-buffer ()
  "Start a shell buffer, but set up MUD variables in it.  Let the user run telnet.  Useful if the machine you're running emacs on doesn't speak to the network, and you don't want to run a separate emacs."
  (interactive)
  (let* ((world (mud-request-world-name))
	 (name (mud-name world))
	 (buf (mud-make-shell name))
	 (proc (put world 'process (get-buffer-process (buffer-name buf)))))
    (set-process-filter proc 'mud-filter)
    (switch-to-buffer buf)
    (newline)
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (mud-interactive-mode world proc)))

(defun mud-make-shell (buffer-name) ;;; this code obsolete in emacs 19
  "Makes or selects a shell buffer in buffer *buffer-name*"
  (let* ((prog (or (and (boundp 'explicit-shell-file-name)
			explicit-shell-file-name)
		   (getenv "ESHELL")
		   (getenv "SHELL")
		   "/bin/sh"))		     
	 (name (file-name-nondirectory prog)))
    (if (get-buffer buffer-name)
	(switch-to-buffer buffer-name)
      (let ((buf (make-comint "mud-temp-progname" prog nil "-i")))
	(switch-to-buffer buf)
	(rename-buffer buffer-name)))
    (get-buffer buffer-name))) ;return buffer to caller


;;; Utilities

(defun mud-cleanup-extra-processes ()
  "Get rid of MUD processes that are still around, but have no buffer
to live in."
  (interactive)
  (mapcar '(lambda (w)
	     (if (not (eq w 0))
		 (let ((p (mud-process w)))
		   (if (and p
			    (not (buffer-name
				  (process-buffer p))))
		       (delete-process p)))))
	  mud-worlds))

(provide 'j-mud)
