(put (mud-world "Lambda MOO") 'word-square "#22285")

(defun moo-source-word-square (square)
  (interactive (list
		(read-from-minibuffer "Source what square: "
				      (save-excursion
					(beginning-of-line)
					(if (looking-at "\\S-* bonks .+ with the word square.  .+ says, \\(\".+\"\\)")
					    (mud-match 1))))))
  (let ((sswg (mud-world-get (mud-world "Lambda MOO") 'word-square)))
    (if sswg
	(mud-send-here (format "source %s with %s" square sswg))
      (error "No sswg known for world %s" (mud-name)))))
