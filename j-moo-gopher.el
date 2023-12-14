;(defvar moo-filter-hook
;  '(moo-filter moo-check-fetch moo-check-gopher mud-check-page
;	       mud-check-reconnect mud-fill-lines))

; (push 'moo-check-gopher moo-filter-hook)

(defvar *moo-gopher-buffer* nil)

(defun moo-check-gopher ()
  "look for a gopher request";
  (goto-char (point-min))
  (while (not (eobp))
    (if (looking-at (concat "#\\$# gopher"
			    "\t\\(.*\\)"
			    "\t\\(.*\\)"
			    "\t\\(.*\\)"
			    "\t\\(.*\\)$"))
	(let ((here (current-buffer))
	      (changed nil)
	      (host (mud-match-field 1))
	      (port (mud-match-field 2))
	      (descr (mud-match-field 3))
	      (path (mud-match-field 4)))
	  (delete-region (point) (save-excursion (beginning-of-line 2)
						 (point)))
	  (if (not (fboundp 'gopher-set-object-host))
	      (load-library "gopher"))
	  (gopher-set-object-host gopher-root-node host)
	  (setq port (car (read-from-string port)))
	  (gopher-set-object-port gopher-root-node port)
	  (save-excursion
	    (gopher-dispatch-object
	     (vector (aref descr 0)
		     (substring descr 1)
		     path
		     host
		     port)
	     *moo-gopher-buffer*)
	    (cond( (not (equal here (current-buffer)))
		   (setq *moo-gopher-buffer* (current-buffer))
		   (setq changed t)
		   ))
	    )
	  (cond (changed
		 (display-buffer *moo-gopher-buffer* t)))
	  )
      (beginning-of-line 2)
      )))
