;;; major mode for programming in MOO-code
;;; by Erik Ostrom (eostrom@nic.gac.edu)
;;; 1992
;;;
;;; for use with mud.el and assorted derivations thereof
;;; to use: load-file this file (or eval-current-buffer)
;;;         M-x moo-code-mode
;;; Make sure your client is loaded _before_ this file, if you want to
;;;   use it interactively (i.e., edit verbs online).
;;; This has been tested with the mud.el available on parcftp.xerox.com
;;;   as of Thu Dec  3 16:40:33 1992.  I have no idea if it will work with
;;;   others.  I suggest you put this file somewhere in your load-path,
;;;   modify your copy of mud.el to
;;;     (require 'moo-code)
;;;   *at the end of the file*,
;;;   and add the line
;;;     (moo-code-mode)
;;;   at the very beginning of the function definition for moo-fix-listing.
;;; It would also be nice to have moo-code-mode interact nicely with
;;;   MOO's "local editing" facility, but that's more complicated.

(defconst moo-code-reserved-words
  '(("if[ (]" "for[ (]" "while[ (]" "fork[ (]" "try"    "else" "except"
     "finally")
    ("endif"  "endfor"  "endwhile"  "endfork"  "endtry" "else" "except"
     "finally")))

(define-prefix-command 'moo-code-extras-map)
(let ((map 'moo-code-extras-map))
  (define-key map "i"  'moo-code-if)
  (define-key map "e"  'moo-code-else)
  (define-key map "f"  'moo-code-for)
  (define-key map "w"  'moo-code-while)
  (define-key map "k"  'moo-code-fork)
  (define-key map "r"  'moo-code-return)
  (define-key map "s"  'moo-code-sin)
  (define-key map "c"  'moo-code-commentify)
  (define-key map "u"  'moo-code-uncommentify))

(defvar moo-code-mode-map
  (let ((map (copy-keymap (cond ((boundp 'moo-macro-mode-map)
				 moo-macro-mode-map)  ; kluge for diff versions
				((boundp 'mud-macro-expansion-mode-map)
				 mud-macro-expansion-mode-map)
				(t
				 (make-sparse-keymap))))))
    (define-key map "\t"        'moo-code-indent-line)
    (define-key map "\C-c\C-a"  'moo-code-extras-map)
    (define-key map "\C-c\""    'moo-code-insert-quoted-end)
    (define-key map "\C-c\;"    'moo-code-check-semi-colons)
    map)
  "Extra keys used in MOO-code mode.")

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

(defvar mud-mode-syntax-table nil
  "Syntax table used while in MUD mode.")

(defun moo-code-mode ()
  "Major mode for mucking with MOO code.
Commands:
\\{moo-code-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "MOO-Code")
  (setq major-mode 'moo-code-mode)
  (if (null mud-mode-syntax-table)
      (progn
	(setq mud-mode-syntax-table (make-syntax-table))
	(set-syntax-table mud-mode-syntax-table)
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?\[ "(]`")
	(modify-syntax-entry ?\] ")['"))
    (set-syntax-table mud-mode-syntax-table))
  (use-local-map moo-code-mode-map)
  (make-local-variable 'mud-expansion-macro-name)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'moo-code-indent-line)
  (message "Use ^C^S to send, ^C^C to send and destroy, ^C^] to abort..."))
(if (boundp 'mud-macro-modes)
    (setq mud-macro-modes
	  (cons 'moo-code-mode
		mud-macro-modes)))

(defun moo-code-return (expression)
  (interactive "sExpression: ")
  (moo-code-indent-line)
  (insert "return (" expression ");\n")
  (moo-code-indent-line))

(defun moo-code-for (variable set)
  (interactive "sVariable: \nsIn set: ")
  (if (not (or (eq (elt set 0) ?\()
	       (eq (elt set (1- (length set))) ?\))
	       (eq (elt set 0) ?\[)
	       (eq (elt set (1- (length set))) ?\])))
      (setq set (format (if (string-match ".+\\.\\..+" set)
			    "[%s]"
			  "(%s)")
			set)))
  (moo-code-insert-statement "for" (concat " " variable " in " set)))
	  
(defun moo-code-fork (seconds)
  (interactive "sSeconds: ")
  (moo-code-insert-statement "fork" (concat " (" seconds ")")))

(defun moo-code-while (condition)
  (interactive "sCondition: ")
  (moo-code-insert-statement "while" (concat " (" condition ")")))

(defun moo-code-if (condition)
  (interactive "sCondition: ")
  (moo-code-insert-statement "if" (concat " (" condition ")")))

(defun moo-code-insert-statement (statement argument)
  (moo-code-indent-line)
  (insert statement argument "\n")
  (moo-code-indent-line)
  (save-excursion
    (newline)
    (insert "end" statement)
    (moo-code-indent-line)
    (if (not (looking-at "\n"))
	(progn
	  (newline)
	  (moo-code-indent-line)))))

(defun moo-code-sin (delay message)
  (interactive "nDelay: \nsMessage: ")
  (insert "$command_utils:suspend_if_needed(" (int-to-string delay)
	  (if (eq (length message) 0)
	      ""
	    (concat ", " (prin1-to-string message)))
	  ");"))

(defun moo-code-else (elseif)
  (interactive "P")
  (if elseif (call-interactively 'moo-code-elseif)
    (progn
      (back-to-indentation)
      (insert "else")
      (moo-code-indent-line)
      (newline)
      (moo-code-indent-line))))

(defun moo-code-elseif (condition)
  (interactive "sCondition: ")
  (insert "elseif (" condition ")")
  (moo-code-indent-line)
  (insert "\n")
  (moo-code-indent-line))

(defun moo-code-indent-line ()
  (interactive)
  (let* ((pos (- (point-max) (point)))
	 (orig (point-marker))
	 (gotoindent (progn (back-to-indentation)
			    (>= (point) orig))))
    (if (not (looking-at "^\\.$"))
	(indent-to
	 (let ((offset 0))
	   (delete-horizontal-space)
	   (if (memq t (mapcar 'looking-at (nth 1 moo-code-reserved-words)))
	       (setq offset -2))
	   (save-excursion
	     (if (not (eq (forward-line -1) -1))
		 (progn
		   (while (and (looking-at "^\\s-*$")
			       (not (eq (forward-line -1) -1))))
		   (back-to-indentation)
		   (if (memq t (mapcar 'looking-at
				       (car moo-code-reserved-words)))
		       (setq offset (+ 2 offset)))
		   (+ (current-indentation) offset))
	       0)))))
    (if gotoindent
	(back-to-indentation)
      (goto-char orig))))

(defun moo-code-commentify (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (end-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (save-restriction
      (narrow-to-region start end)
      (mud-perform-replace "\\" "\\\\")
      (mud-perform-replace "\"" "\\\"")
      (while (re-search-forward "^.*$" nil t)
	(back-to-indentation)
;	(beginning-of-line)
	(insert "\"")
	(end-of-line)
	(insert "\";")))))
	
(defun moo-code-uncommentify (start end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (end-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "^.*$" nil t)
	(back-to-indentation)
	(delete-char 1)
	(end-of-line)
	(delete-char -2))
      (goto-char start)
      (mud-perform-replace "\\\\" "\1")
      (mud-perform-replace "\\\"" "\"")
      (mud-perform-replace "\1"   "\\"))))

(defun moo-code-insert-quoted-end ()
  (interactive)
  (insert ", \".\");"))

(defun moo-code-check-semi-colons ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (eq (char-after (point)) ?@)
      (forward-line 1))
    (while (re-search-forward "^.+$" nil t)
      (back-to-indentation)
      (if (memq t (mapcar 'looking-at
			  (apply 'append moo-code-reserved-words)))
	  (if (and (progn
		     (end-of-line)
		     (eq (char-after (1- (point))) ?\;))
		   (sit-for 1)
	       (y-or-n-p "Inappropriate semicolon.  Delete? "))
	      (delete-char -1))
	(if (and (prog1
		     (not (looking-at "$"))
		   (end-of-line))
		 (progn
		   (not (memq (char-after (1- (point))) '(?\; ?.))))
		 (sit-for 1)
		 (y-or-n-p "Missing semicolon.  Insert? "))
	    (insert ";")))))
  (message "Done."))

(provide 'moo-code)
(provide 'j-moo-code)
	  