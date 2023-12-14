;;; JUPITER
(require 'j-moo)

(defvar mud-jupiter-client-prog "/project/jupiter/bin/jswitch"
  "*File name of the special client program for Jupiter servers.")

(mud-subtype "Jupiter" "MOO"
  '(filters . jupiter-filter-hook)
  '(start-process . (lambda (name buf site port)
		      (start-process "MUD" buf mud-jupiter-client-prog
				     "-n" "-t" name site
				     (int-to-string port))))
  '(sentinel . jupiter-sentinel-hook)) ;;; not used, huh

(defvar jupiter-filter-hook
  (cons 'jupiter-filter moo-filter-hook))
(defvar jupiter-process nil
  "Process variable for mooaudio program.")
(make-variable-buffer-local 'jupiter-process)
(defconst jupiter-mooaudio "/project/jupiter/etc/mooaudio")

(defun jupiter-filter ()
  "Filter room change strings."
  (goto-char (point-min))
  (if (re-search-forward "^@@#\\([0-9]*\\)\n" (point-max) t)
      (let ((room (buffer-substring (match-beginning 1) (match-end 1))))
	(jupiter-set-room room)
	(delete-region (match-beginning 0) (match-end 0))))
  (goto-char (point-min))
  (if (re-search-forward "^#\\$# This server supports fancy clients.\n"
			 (point-max) t)
      (progn
	(process-send-string (get-buffer-process (current-buffer)) "@client emacs\n")
	(delete-region (match-beginning 0) (match-end 0))))
  (goto-char (point-min))
  (if (re-search-forward "^#\\$#channel \\([\.0-9]*\\)\n" (point-max) t)
      (let ((channel (buffer-substring (match-beginning 1) (match-end 1))))
	(jupiter-set-channel channel)
	(delete-region (match-beginning 0) (match-end 0)))))

(defun jupiter-set-room (room)
  (jupiter-set-channel (concat "224.4." room)))

(defun jupiter-set-channel (channel)
  (if (or (null jupiter-process)
	  (not (eq (process-status jupiter-process) 'run)))
      (setq jupiter-process
	    (start-process "jupiter-audio" nil
			   jupiter-mooaudio channel))
      (process-send-string jupiter-process (concat "g " channel "\n"))))


(defun jupiter-sentinel ()
  (if (and (not (eq (process-status proc) 'run))
	   (not (null jupiter-process)))
      (process-send-eof jupiter-process)))

(provide 'j-jupiter)
