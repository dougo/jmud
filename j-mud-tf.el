(defun mud-tf-report-syntax-error ()
  (let ((start (point)))
    (end-of-line)
    (error (concat "Syntax error in MUD world-file " file ": "
		   (buffer-substring start (point))))))

(defun mud-tf-world-pattern (keyword nargs)
  (let ((pattern "?$"))
    (while (> nargs 0)
      (setq pattern (concat "\\([^ \n]*\\) " pattern)
	    nargs (1- nargs)))
    (if (null keyword)
	pattern
      (concat keyword " " pattern))))

(defun mud-translate-tf-file (name)
  "Read worlds from world-file NAME.
Normally called by mud-check-world-file if we haven't got any worlds yet,
or if the file has been changed since we last read it, with NAME set to
mud-world-file.

Syntax is kind of twisted, so that a .moo_worlds file can be read by TinyFugue,
JoeFeedback's non-emacs client of choice:

The command
   /settype <type>
where <type> is the name of any of the known MUD types--do
<help> v mud-types RET to get a list--signifies to moo.el that the
following mud world definitions (/addworld) are of type <type>.

The /addworld command has several uses:

   /addworld default <default login> <default password>
specifies a default login and password for MUDs.

   /addworld <name> <login> <password> <site> <port> [<macro file>]
defines world <name>.  <macro file> is not used by moo.el.

   /addworld <name> <site> <port> [<macro file>]
defines world <name> with the default login and password.

The /c command indicates that the rest of the line is a comment and should
be ignored.

Finally, your world-file should begin with the lines
   /def settype
   /def c
and end with the lines
   /undef c
   /undef settype
.  This is just to keep tinyfugue happy; if you don't want to use tf, you
can skip it.

There should be a sample world-file available wherever you got this; if not,
try cs.williams.edu:pub/uploads/MUD/world-file.  Looking at one could make
this a lot easier."
  (interactive "fWorld-file to translate: ")
  (let ((file (expand-file-name name))
	(list-buffer (generate-new-buffer "*Tinyfugue worlds*"))
	(out-buffer (generate-new-buffer "*MUD worlds*"))
	(type "global"))
    (set-buffer list-buffer)
    (buffer-flush-undo list-buffer)
    (insert-file-contents file)
    ;; Don't lose if no final newline.
    (goto-char (point-max))
    (or (eq (preceding-char) ?\n)
	(newline))
    (goto-char (point-min))
    ;; handle "\\\n" continuation lines
    (while (not (eobp))
      (end-of-line)
      (cond ((= (preceding-char) ?\\)
	     (delete-char -1)
	     (delete-char 1)
	     (insert ?\ )))
      (forward-char 1))
    ;; simplify whitespace handling
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (not (eobp))
      (cond ((or (eolp) (looking-at "/def") (looking-at "/undef") (looking-at "/c")))
	    ((or (looking-at (mud-tf-world-pattern
			      "/addworld default" 2))
		 (looking-at (mud-tf-world-pattern
			      "/addworld default" 3)))
	     (let ((name (mud-match 1))
		   (password (mud-match 2)))
	       (set-buffer out-buffer)
	       (insert "(mud-world-set-pairs \"global\"\n"
		       "          '((login " (prin1-to-string login) "))\n"
		       "          '((password " (prin1-to-string password) ")))\n\n")
	       (set-buffer list-buffer)))
	    ((or (looking-at (mud-tf-world-pattern "/addworld" 5))
		 (looking-at (mud-tf-world-pattern "/addworld" 6)))
	     (let* ((name (mud-match 1))
		    (login (mud-match 2))
		    (password (mud-match 3))
		    (site (mud-match 4))
		    (port (string-to-int (mud-match 5)))
		    (macro-file (mud-match 6))
		    (type (or (symbol-name (mud-parent (mud-world name)))
			      type)))
	       (if (= port 0)
		   (mud-tf-report-syntax-error))
	       (set-buffer out-buffer)
	       (insert "(mud-add-world " (prin1-to-string name)
		       " " (prin1-to-string type) "\n"
		       "               " (prin1-to-string login)
		       " " (prin1-to-string password) "\n"
		       "               " (prin1-to-string site)
		       " " (int-to-string port) "\n"
		       "               " (prin1-to-string macro-file) ")\n\n")
	       (set-buffer list-buffer)))
	    ((or (looking-at (mud-tf-world-pattern "/addworld" 3))
		 (looking-at (mud-tf-world-pattern "/addworld" 4)))
	     (let* ((name (mud-match 1))
		    (site (mud-match 2))
		    (port (string-to-int (mud-match 3)))
		    (macro-file (mud-match 4))
		    (type (or (symbol-name (mud-parent (mud-world name)))
			      type)))
	       (if (= port 0)
		   (mud-tf-report-syntax-error))
	       (set-buffer out-buffer)
	       (insert "(mud-add-world " (prin1-to-string name)
		       " " (prin1-to-string type) "\n"
		       "               nil nil\n"
		       "               " (prin1-to-string site)
		       " " (int-to-string port) "\n"
		       "               " (prin1-to-string macro-file) ")\n\n")
	       (set-buffer list-buffer)))
	    ((looking-at (mud-tf-world-pattern "/settype" 1))
	     (setq type (mud-match 1))
	     (let* ((type-symbol (mud-world type))
		    (parent-name (if type-symbol (mud-name (mud-parent
							    type-symbol))
				   "global")))
	       (set-buffer out-buffer)
	       (insert "(mud-subtype " (prin1-to-string type)
		       " " (prin1-to-string parent-name) ")\n\n")
	       (set-buffer list-buffer)))
	    (t
	     (mud-tf-report-syntax-error)))
      (forward-line))
    (kill-buffer list-buffer)
    (pop-to-buffer out-buffer)
    (message "Parsed Tinyfugue list, please edit and save.")))

	  
