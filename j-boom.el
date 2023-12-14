;;; General features that depend on a browser object
(provide 'j-boom)

(require 'j-boom-obj)
(require 'j-boom-tree)
(require 'j-mud)

(defun boom-rename (objnum name)
  "Rename objnum in tree buffer, objnum buffer, and objnum:verb buffers,
if each exists.  Doesn't do cross-referencing, yet."
  (let ((buffer-names (concat "\\*" objnum "\\(:.*\\)@"
			      (symbol-name mud-here) "*"))
	buf)
    (save-excursion
      (if (setq buf (get-buffer (concat "*Browser: " (symbol-name mud-here) "*")))
	  (progn
	    (set-buffer buf)
	    (moo-tree-jump-to-object objnum)
	    (save-restriction
	      (narrow-to-region (point) (progn (end-of-line) (point)))
	      (mud-perform-replace (boom-tree-name) name))))

      (mapcar 'boom-obj-rename-object-in-buffer
	      (mapcar '(lambda (buff) (if (string-match buffer-names
							(buffer-name buff))))
		      (buffer-list))))))

