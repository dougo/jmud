;;; upload files in various ways
(require 'prefix)

(defun mud-upload-file (file-name header prefix suffix footer send-now)
  "Send a file to the MUD.  Args are FILE-NAME, name of file to get; HEADER,
string to send before the file; PREFIX, string to prepend to each line of
the file before sending; SUFFIX, string to append to each line; and FOOTER,
string to send after the file.  If universal-prefix (\\[universal-prefix]) or
argument send-now, sends directly to process; otherwise, opens buffer for
editing."
  (interactive "fFrom file: \nsHeader: \nsPrefix string: \nsSuffix string: \nsFooter: \np")
  (mud-upload-buffer file-name header prefix suffix footer send-now t))

(defun mud-upload-file-straight (file-name send-now)
  "Send a file from disk to a MUD."
  (interactive "fFrom file: \nP")
  (mud-upload-file file-name "" "" "" "" send-now))

(defun mud-upload-file-as-lambda-mail (file-name recipient subject send-now)
  "Send a file from disk to a MUD as a mail message to RECIPIENT."
  (interactive "fFrom file: \nsRecipient: \nsSubject: \nP")
  (mud-upload-file file-name (concat "@send " recipient "\n" subject) "\"" "" "send" send-now))


(defun mud-upload-buffer (what header prefix suffix footer
			       send-now &optional file) 
  "Send a buffer to the MUD.  Args are WHAT (which buffer to send, or
a file name if called from a function with optional sixth arg FILE);
HEADER, string to send before the buffer; PREFIX, string to prepend to
each line of the buffer before sending; SUFFIX, string to append to
each line; and FOOTER, string to send after the buffer.  If
universal-prefix (\\[universal-prefix]) or argument send-now, sends directly
to process; otherwise, opens buffer for editing.  As mentioned,
optional arg FILE tells mud-upload-buffer to insert a file, rather
than a buffer."
  (interactive "bBuffer: \nsHeader: \nsPrefix string: \nsSuffix string: \nsFooter: \nP")
  (let ((buf (current-buffer))
	(name (concat (symbol-name mud-here) ": "
		       (if (stringp what) what (buffer-name what)))))
    (save-excursion
      (set-buffer (get-buffer-create name))
      (erase-buffer)
      (if file
	  (insert-file-contents what)
	(insert-buffer-substring what))
      (surround-region header prefix suffix footer (point-min) (point-max))
      (if send-now
	  (if (and (re-search-forward "~" nil t)
		   (not (y-or-n-p
			 "At least one tilde in this buffer.  Send anyway?")))
	      (setq send-now nil)
	    (progn
	      (mud-copy-here-from buf)
	      (mud-macro-send-and-destroy)
	      (set-buffer buf))))
      (if (null send-now)
	  (progn
	    (message (prin1-to-string (current-buffer)))
	    (sit-for 2)
	    (mud-macro-expansion-mode)
	    (mud-copy-here-from buf)
	    (display-buffer (current-buffer)))))))
    
    
