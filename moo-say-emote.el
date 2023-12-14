(defun moo-say-emote (times)
  "Minibuffer version of say, emote and other single letter commands."
  (interactive "p") 
  (if (or (bolp) (= (preceding-char) (mud-prompt mud)))
      (let* ((which last-command-char)
	     (text  (read-string (char-to-string which)))
	     (firstword (progn (string-match "[ \t]+" (concat text " "))
			       (1- (match-end 0)))))
	(mud-send-string
	 (concat (char-to-string which)
		 (if (memq which '(?: ?\" ?;))
		     ;;; moo server makes special exception for these 3 chars
		     text
		   (concat 
		    (mapconcat (function (lambda (c) (list ?\\ c)))
			       (substring text 0 (max 0 firstword)) "")
		    " "
		    (substring text (min (length text) (1+ firstword))))))
	 (get-buffer-process (current-buffer))))
    (self-insert-command times)))

