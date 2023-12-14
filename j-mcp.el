;;; moo-client protocol

(require 'j-moo)
(require 'j-moo-code)

(defvar mcp-message-regexp (concat "^#\\$#"                ; out-of-band tag
				   "\\([^ :*\\\"]+\\)"     ; message-name
				   "\\( +\\([^ :\\\"]+\\)\\( \\|$\\)\\)?"
					                   ; authentication-key
				   "\\( *\\(.*\\)\\)$"))  ; key-value pairs

(defvar mcp-multifield-regexp (concat "^#\\$#"         ; out-of-band tag
				      "\\*"            ; signals multifield
				      " +\\([^ ]+\\)"  ; message tag
				      " +\\([^ ]+\\):" ; field tag
				      " \\(.*\\)$"))   ; data line

(defvar mcp-end-regexp (concat "^#\\$#"                ; out-of-band tag
			       ":"                     ; signals end
			       " +\\([^ ]+\\)$"))      ; message tag

(defun auth-ok-p (authkey)
  (string= authkey mcp-auth-key))

(defun mcp-handle-message (msgname authkey keyval-string)
  "Figure out if we know what to do with the given message;
check the authkey if it's important;
check that we have all the args we want;
if data-follows, start gathering it."
  (let ((keyvals (mcp-parse-keyvals keyval-string)))
    (if (and (listp keyvals)
	     (or (auth-ok-p authkey)
		 (string= msgname "mcp")))
	(let* ((type (mcp-lookup-msgtype msgname))
	       (message (mcp-message type keyvals)))
;;	  (mcp-remove-line)
	  (if type
	      (if (mcp-message-incomplete message)
		  (let ((datatag (assoc '_data-tag keyvals)))
		    (if datatag
			(mcp-handle-unfinished message (cdr datatag))))
		(mcp-dispatch-message message)))))))

(defvar mcp-keyval-regexp (concat "\\([^ *\\\":]+\\)"    ; keyword
				  "\\(\\*\\)?: +"       ; (multiline)
				  "\\([^ \\\"]+\\|"     ; value
				  "\"\\([^\\\"]\\|\\\\.\\)*\"\\)"
					                ; quoted value
				  "\\( +\\|$\\)")       ; rest
  "Recognize one keyword: value pair.")

(defun mcp-parse-keyvals (keyval-string)
  (catch 'mcp-failed-parse
    (let ((arglist nil)
	  (start 0))
      (while (< start (length keyval-string))
	(if (string-match mcp-keyval-regexp keyval-string start)
	    (setq start (match-end 0)
		  arglist (cons
			   (cons
			    (intern (mud-match-string 1 keyval-string))
			    (if (match-beginning 2)
				'()
			      (if (eq (elt keyval-string
					   (match-beginning 3))
				      ?\")
				  (car (read-from-string
					(mud-match-string 3 keyval-string)))
				(mud-match-string 3 keyval-string))))
			   arglist))
	  (throw 'mcp-failed-parse 'mcp-failed-parse)))
      arglist)))
				
(defun mcp-remove-line ()
  (let ((start (progn (mud-beginning-of-line) (point))))
    (beginning-of-line 2)
    (delete-region start (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      ;;;
;;; MCP 2 implementation ;;;
;;;                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; version data type (major, minor)

(fset 'mcp-version #'cons)
(fset 'mcp-version-major #'car)
(fset 'mcp-version-minor #'cdr)
(fset 'mcp-no-version-p #'not)
(fset 'mcp-version= #'equal)
(defvar mcp-no-version nil)

(defvar mcp-version-regexp "^\\([0-9]+\\)\\.\\([0-9]+\\)$")

(defun mcp-read-version (str)
  (let ((match (string-match mcp-version-regexp str)))
    (if match
	(mcp-version (string-to-number (mud-match-string 1 str))
		     (string-to-number (mud-match-string 2 str)))
      mcp-no-version)))

(defun mcp-print-version (v)
  (format "%d.%d" (mcp-version-major v) (mcp-version-minor v)))

(defun mcp-version-<= (v1 v2)
  (or (< (car v1) (car v2))
      (and (= (car v1) (car v2))
	   (<= (cdr v1) (cdr v2)))))

(defun mcp-version-min (v1 v2)
  (if (mcp-version-<= v1 v2)
      v1
    v2))

(defun mcp-version-max (v1 v2)
  (if (mcp-version-<= v1 v2)
      v2
    v1))

;;; version range data type (min, max)

(fset 'mcp-version-range #'cons)
(fset 'mcp-version-range-min #'car)
(fset 'mcp-version-range-max #'cdr)
(fset 'mcp-empty-version-range-p #'not)
(defvar mcp-empty-version-range nil)

(defun mcp-version-range-overlap (vr1 vr2)
  (if (or (mcp-empty-version-range-p vr1) (mcp-empty-version-range-p vr2))
      mcp-empty-version-range
    (let ((max-of-mins (mcp-version-max (mcp-version-range-min vr1)
					(mcp-version-range-min vr2)))
	  (min-of-maxs (mcp-version-min (mcp-version-range-max vr1)
					(mcp-version-range-max vr2))))
      (if (mcp-version-<= max-of-mins min-of-maxs)
	  (mcp-version-range max-of-mins min-of-maxs)
	mcp-empty-version-range))))

;;; package data type

(defun mcp-package (name parent version-range initfunc
			 msgtypes &optional cordtypes)
  (let ((package (vector name parent version-range initfunc
			 msgtypes cordtypes)))
    (mapcar #'(lambda (cordtype) (mcp-cordtype-set-package cordtype package))
	    cordtypes)
    package))
(defun mcp-package-name (package) (aref package 0))
(defun mcp-package-parent (package) (aref package 1))
(defun mcp-package-version-range (package) (aref package 2))
(defun mcp-package-initfunc (package) (aref package 3))
(defun mcp-package-msgtypes (package) (aref package 4))
(defun mcp-package-cordtypes (package) (aref package 5))

(defvar mcp-root-package nil)

(defun mcp-concat-hyphenated (a b)
  (concat a
	  (if (or
	       (string= a "") (string= b ""))
	      ""
	    "-")
	  b))

(defun mcp-package-fullname (package)
  (let ((parent (mcp-package-parent package)))
    (mcp-concat-hyphenated (if parent (mcp-package-fullname parent) "")
			   (mcp-package-name package))))

;;; package registry

(defconst mcp-implemented-packages '())

(defun mcp-register-package (package)
  (let ((packages mcp-implemented-packages)
	(done nil))
    (while packages
      (if (eq (mcp-package-name (car packages))
	      (mcp-package-name package))
	  (progn
	    (setcar packages package)
	    (setq done nil)
	    (setq packages nil))
	(setq packages (cdr packages))))
    (if (not done)
	(setq mcp-implemented-packages
	      (cons package mcp-implemented-packages))))
  (mapcar #'(lambda (msgtype) (mcp-register-msgtype package msgtype))
	  (mcp-package-msgtypes package))
  (mapcar #'(lambda (cordtype) (mcp-register-cordtype cordtype))
	  (mcp-package-cordtypes package)))

(defun mcp-lookup-package (pkgname)
  (catch 'mcp-lookup-package
    (mapcar #'(lambda (package)
		(if (string= (mcp-package-fullname package) pkgname)
		    (throw 'mcp-lookup-package package)))
	    mcp-implemented-packages)
    nil))

(defun mcp-register-package-shortcut (name parent quickrange initfunc
					   msgdefs &optional corddefs)
  (mcp-register-package
   (mcp-package name parent
		(mcp-version-range
		 (mcp-read-version (car quickrange))
		 (mcp-read-version (car (cdr quickrange))))
		initfunc
		(mcp-map-apply 'mcp-msgtype msgdefs)
		(mcp-map-apply #'(lambda (name msgdefs
					  &optional openfunc closefunc)
				   (mcp-cordtype name
						 (mcp-map-apply 'mcp-msgtype
								msgdefs)
						 openfunc closefunc))
			       corddefs))))

;;; message type data type

(defun mcp-msgtype (name function args)
  (vector name function args))
(defun mcp-msgtype-name (msgtype) (aref msgtype 0))
(defun mcp-msgtype-function (msgtype) (aref msgtype 1))
(defun mcp-msgtype-args (msgtype) (aref msgtype 2))

(defun mcp-msgtype-fullname (msgtype package)
  (mcp-concat-hyphenated (mcp-package-fullname package)
			 (mcp-msgtype-name msgtype)))

;;; message type registry

(defvar mcp-msgtypes '())

(defvar mcp-unknown-msgtype
  (mcp-msgtype 'unknown
	       #'(lambda (message) "drop on floor")
	       '()))

(defun mcp-register-msgtype (package msgtype)
  (let* ((fullname (mcp-msgtype-fullname msgtype package))
	 (a (assoc fullname mcp-msgtypes)))
    (if a
	(setcdr a msgtype)
      (setq mcp-msgtypes (cons (cons fullname msgtype) mcp-msgtypes)))))

(defun mcp-lookup-msgtype (msgname)
  (let ((a (assoc msgname mcp-msgtypes)))
    (if a
	(cdr a)
      mcp-unknown-msgtype)))

;;; message data type

(defun mcp-message (type args)
  (vector type args))
(defun mcp-message-type (message) (aref message 0))
(defun mcp-message-args (message) (aref message 1))

(defun mcp-message-get (message key &rest default)
  (let ((pair (assq key (mcp-message-args message))))
    (if pair
	(cdr pair)
      (if default
	  (car default)
	(throw 'mcp-message-missing-arg t)))))

(defun mcp-message-append (message key line)
  "Append a line to a given argument of an MCP message."
  (let ((pair (assq key (mcp-message-args message))))
    (if pair
	(setcdr pair (append (cdr pair) (list line)))
      (error "No %s keyword in MCP %s message" key
	     (mcp-msgtype-name (mcp-message-type message))))))

(defun mcp-message-check-args (message)
  (catch 'mcp-message-missing-arg
      (let ((requireds (mcp-msgtype-args (mcp-message-type message)))
	    (provideds (mcp-message-args message)))
	(mapcar #'(lambda (required)
		    (apply 'mcp-message-get message required))
		requireds))))

(defun mcp-message-incomplete (message)
  "Is accurate only when called on a just-formed message."
  (catch 'mcp-message-incomplete
    (mcp-map-alist #'(lambda (key value)
		       (if (listp value)
			   (throw 'mcp-message-incomplete t)))
		   (mcp-message-args message))
    nil))

(defun mcp-message-send (world message)
  (mcp-send world
	    (mcp-msgtype-name (mcp-message-type message))
	    (mcp-message-args message)))

;;; MCP procedures

(defvar mcp-auth-key nil)
(defvar mcp-running-version mcp-no-version)
(defvar mcp-running-packages nil)

(defvar mcp-implemented-version-range
  (mcp-version-range (mcp-version 2 1) (mcp-version 2 1)))

(defun mcp-determine-version (min-version max-version)
  (let* ((server-range (mcp-version-range (mcp-read-version min-version)
					  (mcp-read-version max-version)))
	 (overlap (mcp-version-range-overlap mcp-implemented-version-range
					     server-range)))
    (if (mcp-empty-version-range-p overlap)
	mcp-no-version
      (mcp-version-range-max overlap))))

(defun mcp-send-packages (world)
  (mapcar #'(lambda (package)
		(mcp-send-negotiate world package))
	  mcp-implemented-packages)
  (mcp-send world 'mcp-negotiate-end '()))

(defun mcp-map-alist (function alist)
  (mapcar #'(lambda (pair)
	      (funcall function (car pair) (cdr pair)))
	  alist))

(defun mcp-map-apply (function alist)
  (mapcar #'(lambda (entry)
	      (apply function entry))
	  alist))

(defun mcp-send (world message alist)
  (let ((proc (mud-process world))
	(multis '())
	(datatag (number-to-string (random 100000000))))
    (process-send-string proc (format "#$#%s" message))
    (if (not (eq message 'mcp))
	(process-send-string proc (format " %s" (mud-world-get world 'mcp-auth-key))))
    (mcp-map-alist #'(lambda (key value)
		       (let ((multiline (listp value)))
			 (if multiline
			     (setq multis (cons (cons key
						      value)
						multis)))
			 (process-send-string proc
				      (format " %s%s: %S"
					      key
					      (if multiline "*" "")
					      (if multiline "" value)))))
		   alist)
    (if multis
	(process-send-string proc (format " _data-tag: %s" datatag)))
    (process-send-string proc "\n")
    (if multis
	(progn
	  (mcp-map-alist #'(lambda (key value)
			     (mapcar #'(lambda (line)
					 (process-send-string
					  proc
					  (format "#$#* %s %s: %s\n"
						  datatag key line)))
				     value))
			 multis)
	  (process-send-string proc (format "#$#: %s\n" datatag))))))

;;; multifield message handling
(defvar mcp-messages '())

(defun mcp-lookup-message (datatag &optional final)
  "Look up the given DATATAG in this buffer's list of pending messages.
If optional arg FINAL is true, remove the message from the list."
  (let ((pair (assoc datatag mcp-messages)))
    (if (and final pair)
	(setq mcp-messages (delq pair mcp-messages)))
    (if pair
	(cdr pair)
      nil)))

(defun mcp-handle-unfinished (message datatag)
  (setq mcp-messages (cons (cons datatag message) mcp-messages)))

(defun mcp-handle-multifield (datatag ftag data) ; see mcp-multifield-regexp
  (let ((message (mcp-lookup-message datatag)))
    (if message
	(mcp-message-append message (intern ftag) data))))

(defun mcp-handle-end (datatag) ; see mcp-end-regexp
  (let ((message (mcp-lookup-message datatag 'final)))
    (if message
	(mcp-dispatch-message message))))

(defun mcp-dispatch-message (message)
  (let ((arglist (mcp-message-check-args message)))
    (if (listp arglist)
	(apply (mcp-msgtype-function (mcp-message-type message))
	       message
	       arglist))))

;;; #$#mcp

(defun mcp-handle-mcp (message min-version max-version)
  (let ((v (mcp-determine-version min-version max-version)))
    (make-local-variable 'mcp-running-version)
    (setq mcp-running-version v)
    (if (not (mcp-no-version-p v))
	(let ((proc (get-buffer-process (current-buffer))))
	  (make-local-variable 'mcp-auth-key)
	  (setq mcp-auth-key (prin1-to-string (random)))
	  (mud-world-set-pair mud-here `(mcp-auth-key . ,mcp-auth-key))
	  (make-local-variable 'mcp-messages)
	  (setq mcp-messages nil)
	  (make-local-variable 'mcp-running-packages)
	  (setq mcp-running-packages nil)
	  (mcp-send mud-here 'mcp
		    `((authentication-key . ,mcp-auth-key)
		      (version . ,(mcp-print-version
				   (mcp-version-range-min
				    mcp-implemented-version-range)))
		      (to . ,(mcp-print-version
			      (mcp-version-range-max
			       mcp-implemented-version-range)))))
	  (mcp-send-packages mud-here)))))

;;; this is a kluge.  really, the #$#mcp message isn't a message in
;;; the standard sense, and it definitely doesn't belong to a package.
;;; but since the syntax matches that of general messages, we treat it
;;; like one (and special-case it elsewhere) to get the convenience of
;;; message spotting and parsing.

(defvar mcp-mcp-msgtype
  (mcp-msgtype "" 'mcp-handle-mcp '((version) (to))))

(defvar mcp-mcp-package
  (mcp-package "mcp" mcp-root-package
	       (mcp-version-range
		(mcp-version 2 1)
		(mcp-version 2 1))
	       nil
	       (list mcp-mcp-msgtype)))

(mcp-register-package mcp-mcp-package)

;;; mcp-negotiate package

(defun mcp-send-negotiate (world package)
  (let ((name (mcp-package-fullname package))
	(range (mcp-package-version-range package)))
    (if (not (string= name "mcp"))
	(mcp-send world 'mcp-negotiate-can
		  `((package . ,name)
		    (min-version
		     . ,(mcp-print-version
			 (mcp-version-range-min range)))
		    (max-version
		     . ,(mcp-print-version
			 (mcp-version-range-max range))))))))

(defun mcp-handle-negotiate-can (message pkgname min-version max-version)
  (let ((package (mcp-lookup-package pkgname)))
    (if package
	(let* ((range (mcp-version-range
		       (mcp-read-version min-version)
		       (mcp-read-version max-version)))
	       (overlap (mcp-version-range-overlap
			 (mcp-package-version-range package)
			 range)))
	  (if (not (mcp-empty-version-range-p overlap))
	      (let ((initfunc (mcp-package-initfunc package)))
		(setq mcp-running-packages
		      (cons (cons package (mcp-version-range-max overlap))
			    mcp-running-packages))
		(if initfunc (funcall initfunc))))))))

(defun mcp-handle-negotiate-end (message)
  "do nothing")

(mcp-register-package-shortcut
 "negotiate"
 mcp-mcp-package
 '("1.0" "2.0")
 nil
 '(("can" mcp-handle-negotiate-can
	((package) (min-version) (max-version)))
   ("end" mcp-handle-negotiate-end ())))

;;; cord type data type
(defun mcp-cordtype (name msgtypes &optional openfunc closefunc)
  (vector name nil msgtypes openfunc closefunc))
(defun mcp-cordtype-name (cordtype) (aref cordtype 0))
(defun mcp-cordtype-package (cordtype) (aref cordtype 1))
(defun mcp-cordtype-msgtypes (cordtype) (aref cordtype 2))
(defun mcp-cordtype-openfunc (cordtype) (aref cordtype 3))
(defun mcp-cordtype-closefunc (cordtype) (aref cordtype 4))

(defun mcp-cordtype-set-package (cordtype package)
  (aset cordtype 1 package))

(defun mcp-cordtype-fullname (cordtype)
  (mcp-concat-hyphenated (mcp-package-fullname (mcp-cordtype-package cordtype))
			 (mcp-cordtype-name cordtype)))

(defun mcp-cordtype-lookup-msgtype (cordtype msgname)
  (catch 'mcp-cordtype-lookup-msgtype
    (mapcar #'(lambda (msgtype)
		(if (string= (mcp-msgtype-name msgtype) msgname)
		    (throw 'mcp-cordtype-lookup-msgtype msgtype)))
	    (mcp-cordtype-msgtypes cordtype))
    nil))

;;; cord type registry
(defvar mcp-cordtypes '())

(defun mcp-register-cordtype (cordtype)
  (catch 'mcp-register-cord-type
    (let ((cordtypes mcp-cordtypes))
      (while cordtypes
	(if (string= (mcp-cordtype-fullname (car cordtypes))
		     (mcp-cordtype-fullname cordtype))
	    (progn
	      (setcar cordtypes cordtype)
	      (throw 'mcp-register-cord-type nil)))))
    (setq mcp-cordtypes (cons cordtype mcp-cordtypes))))

(defun mcp-lookup-cordtype (typename)
  (catch 'mcp-lookup-cordtype
    (mapcar #'(lambda (cordtype)
		(if (string= (mcp-cordtype-fullname cordtype) typename)
		    (throw 'mcp-lookup-cordtype cordtype)))
	    mcp-cordtypes)
    nil))

;;; cord data type
(defun mcp-cord (type id)
  (vector type id nil))
(defun mcp-cord-type (cord) (aref cord 0))
(defun mcp-cord-id (cord) (aref cord 1))
(defun mcp-cord-state (cord) (aref cord 2))

;;; cord message data type

;;; note that mcp-cordmsg is a subtype of mcp-message, i.e., all
;;; operations that can be applied to mcp-message can also be applied
;;; to mcp-cord-message.
(defun mcp-cordmsg (type args cord)
  (vector type args cord))
(defun mcp-cordmsg-type (cordmsg) (aref cordmsg 0))
(defun mcp-cordmsg-args (cordmsg) (aref cordmsg 1))
(defun mcp-cordmsg-cord (cordmsg) (aref cordmsg 2))

(defun mcp-cordmsg-send (world cordmsg)
  (mcp-send world 'mcp-cord
	    (append `((_id . ,(mcp-cord-id (mcp-cordmsg-cord cordmsg)))
		      (_message . ,(mcp-msgtype-name
				    (mco-cordmsg-type cordmsg))))
		    alist)))

;;; mcp-cord package
(defvar mcp-cords '())

(defun mcp-cords-init ()
  (make-local-variable 'mcp-cords)
  (setq mcp-cords '()))

(defun mcp-lookup-cord (id)
  (catch 'mcp-lookup-cord
    (mapcar #'(lambda (cord)
		(if (string= (mcp-cord-id cord) id)
		    (throw 'mcp-lookup-cord cord)))
	    mcp-cords)
    nil))

(defun mcp-handle-cord-open (message id typename)
  (let ((cordtype (mcp-lookup-cordtype typename)))
    (if cordtype
	(let ((cord (mcp-cord cordtype id)))
	  (setq mcp-cords (cons cord mcp-cords))
	  (if (mcp-cordtype-openfunc cordtype)
	      (funcall (mcp-cordtype-openfunc cordtype) cord))))))

(defun mcp-handle-cord-closed (message id)
  (let ((cord (mcp-lookup-cord id)))
    (if cord
	(let ((closefunc (mcp-cordtype-closefunc (mcp-cord-type cord))))
	  (if closefunc
	      (funcall closefunc cord))
	  (setq mcp-cords (delq cord mcp-cords))))))

(defun mcp-handle-cord (message id msgname)
  (let ((cord (mcp-lookup-cord id)))
    (if cord
	(let ((msgtype (mcp-cordtype-lookup-msgtype (mcp-cord-type cord)
						    msgname)))
	  (if msgtype
	      (let ((newmessage
		     (mcp-cordmsg msgtype
				  (mcp-message-args message)
				  cord)))
		(mcp-dispatch-message newmessage)))))))

(defun mcp-send-cord (world cord message alist)
  (mcp-send world 'mcp-cord
	    (append `((_id . ,(mcp-cord-id cord))
		      (_message . ,message))
		    alist)))

(mcp-register-package-shortcut
 "cord" mcp-mcp-package
 '("1.0" "1.0")
 'mcp-cords-init
 '(("open" mcp-handle-cord-open ((_id) (_type)))
   ("closed" mcp-handle-cord-closed ((_id)))
   ("" mcp-handle-cord ((_id) (_message)))))

;;; cord test package

(defun mcp-cord-test-open (cord)
  (message "cord opened: %S" cord))

(defun mcp-cord-test-closed (cord)
  (message "cord closed: %S" cord))

(defun mcp-cord-test-handle (message)
  (message "cord message: %S" message))

;;; mcp-edit protocol
;;; 
;;; based on richard godard's proposal on mcp-dev

(defvar mcp-edit-reference nil)
(defvar mcp-edit-type nil)

(defun mcp-edit-handle-send (message ident name type text)
  (let ((buf (current-buffer))
	(world mud-here)
	(newbuf (create-file-buffer name)))
    (save-excursion
      (set-buffer newbuf)
      (if (string= type "string")
	  (insert text "\n")
	(mapcar #'(lambda (line) (insert line "\n"))
		text))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (if (string= type "moo-code") (moo-code-mode))
      (mcp-edit-mode t)
      (make-local-variable 'mcp-edit-reference)
      (make-local-variable 'mcp-edit-type)
      (setq mcp-edit-reference ident
	    mcp-edit-type type)
      (setq mcp-edit-mode-target world)
      )
    (display-buffer newbuf)))

(defvar mcp-moo-mud-org-package
  (mcp-package "dns-org-mud-moo" mcp-root-package
	       mcp-empty-version-range
	       nil nil))
  
(mcp-register-package-shortcut
 "simpleedit" mcp-moo-mud-org-package
 '("1.0" "1.0")
 nil
 `(("content" mcp-edit-handle-send ((reference) (name) (type) (content)))
   ("set" ,nil ((reference) (type) (content)))))

;;; mcp edit minor mode
;;; 
;;; 

(defvar mcp-edit-mode nil)
(make-variable-buffer-local 'mcp-edit-mode)
(defvar mcp-edit-mode-target nil)

(defun mcp-edit-mode (&optional toggle)
  "Minor mode for editing in an MCP environment."
  (interactive "P")
  (if (listp toggle)
      (setq toggle (car toggle)))
  (setq mcp-edit-mode
	(if toggle
	    (or (and (integerp toggle)
		     (> toggle 0))
		(and (symbolp toggle)
		     (not (eq toggle 'nil))
		     (not (eq toggle '-))))
	  (not mcp-edit-mode)))
  (if mcp-edit-mode
      (make-local-variable 'mcp-edit-mode-target)))

(defun mcp-edit-buffer-to-lines ()
  "Return the contents of the current buffer as a list of strings."
  (save-excursion
    (goto-char (point-max))
    (let ((end (point))
	  (l nil))
      (if (looking-at "^$")
	  (progn
	    (forward-char -1)
	    (setq end (1- end))))
      (while (search-backward "\n" nil 'foo)
	(setq l (cons (buffer-substring (1+ (point)) end) l))
	(setq end (point)))
      (cons (buffer-substring (point) end) l))))

(defun mcp-edit-send ()
  "Send contents of MCP editing buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (mcp-send mcp-edit-mode-target 'dns-org-mud-moo-simpleedit-set
	    `((reference . ,mcp-edit-reference)
	      (type . ,mcp-edit-type)
	      (content . ,(if (string= mcp-edit-type "string")
			      (car (mcp-edit-buffer-to-lines))
			    (mcp-edit-buffer-to-lines))))))

(defvar mcp-edit-recover-buffers nil
  "Buffers that can be recovered if accidentally aborted.")

(defun mcp-edit-abort ()
  "Abort MCP editing buffer."
  (interactive)
  (if (not (memq (current-buffer) mcp-edit-recover-buffers))
      (setq mcp-edit-recover-buffers
	    (cons (current-buffer)
		  mcp-edit-recover-buffers))
    (let ((rec-rev (reverse mcp-edit-recover-buffers)))
      (while (> (length rec-rev) mud-recoverable-macros)
	(kill-buffer (car rec-rev))
	(setq rec-rev (cdr rec-rev)))
      (setq mcp-edit-recover-buffers (reverse rec-rev))))
  (bury-buffer)
  (if mud-delete-macro-window
      (delete-window)))

(defun mcp-edit-recover-last-buffer ()
  (interactive)
  (switch-to-buffer (car mcp-edit-recover-buffers))
  (setq mcp-edit-recover-buffers (cdr mcp-edit-recover-buffers)))

(defun mcp-edit-send-and-destroy ()
  "Send contents of MCP editing buffer and then kill the buffer."
  (interactive)
  (mcp-edit-send)
  (mcp-edit-abort))

(defun mcp-edit-retarget (world)
  "Change world to send this MCP editing buffer to."
  (interactive (list
		(mud-request-world-name)))
  ;; FIXME: should change mode line
  (setq mcp-edit-mode-target world))
  
(defvar mcp-edit-mode-keymap (make-sparse-keymap)
  "Keymap for mcp-edit mode.")
(define-key mcp-edit-mode-keymap "\^c\^s" 'mcp-edit-send)
(define-key mcp-edit-mode-keymap "\^c\^c" 'mcp-edit-send-and-destroy)
(define-key mcp-edit-mode-keymap "\^c\^]" 'mcp-edit-abort)
(define-key mcp-edit-mode-keymap "\^c\^r" 'mcp-edit-retarget)

(setq minor-mode-map-alist
      (cons `(mcp-edit-mode . ,mcp-edit-mode-keymap)
	    minor-mode-map-alist))

(if (not (assq 'mcp-edit-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(mcp-edit-mode " MCP-Edit") minor-mode-alist)))

;;; display-url package
;;; 
;;; after janus's specification

(require 'browse-url)

(defun mcp-display-url-handle (message url target)
  ;; 'target' is supposed to be the name of the frame to open the url
  ;; in, but we can't do anything with that without writing our own
  ;; URL api.
  (funcall browse-url-browser-function url))

(mcp-register-package-shortcut
 "display-url" mcp-root-package
 '("1.0" "1.0")
 nil
 '(("" mcp-display-url-handle ((url) (target nil)))))

(provide 'j-mcp)
