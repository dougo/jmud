;;; Input History Maintenance
(defun mud-make-history (size)
  ;; (head tail . vector)
  ;; head is the index of the most recent item in the history.
  ;; tail is the index one past the oldest item
  ;; if head == tail, the history is empty
  ;; all index arithmetic is mod the size of the vector
  (cons 0 (cons 0 (make-vector (+ size 1) nil))))

(defun mud-decr-mod (n m)
  (if (= n 0)
      (1- m)
    (1- n)))

(defun mud-history-insert (history element)
  (let* ((head (car history))
	 (tail (car (cdr history)))
	 (vec (cdr (cdr history)))
	 (size (length vec))
	 (new-head (mud-decr-mod head size)))
    (aset vec new-head element)
    (setcar history new-head)
    (if (= new-head tail)  ; history is full, so forget oldest element
	(setcar (cdr history) (mud-decr-mod tail size)))))

(defun mud-history-empty-p (history)
  (= (car history) (car (cdr history))))

(defun mud-history-ref (history index)
  (let* ((head (car history))
	 (tail (car (cdr history)))
	 (vec (cdr (cdr history)))
	 (size (if (<= head tail)
		   (- tail head)
		 (+ tail (- (length vec) head)))))
    (if (= size 0)
	(error "Ref of an empty history")
      (let ((i (% index size)))
	(if (< i 0)
	    (setq i (+ i size)))
	(aref vec (% (+ head i) (length vec)))))))

(defvar mud-input-history-size 20
  "The number of past input commands remembered for possible reuse")

(defvar mud-input-history nil)

(defvar mud-input-index 0)

(defun mud-initialize-input-history ()
  (make-local-variable 'mud-input-history)
  (make-local-variable 'mud-input-index)
  (setq mud-input-history (mud-make-history mud-input-history-size))
  (setq mud-input-index 0))

(defun mud-remember-input (string)
  (mud-history-insert mud-input-history string))

(defun mud-previous-command ()
  (interactive)
  (mud-browse-input-history 1))

(defun mud-next-command ()
  (interactive)
  (mud-browse-input-history -1))

(defun mud-browse-input-history (delta)
  (cond ((mud-history-empty-p mud-input-history)
	 (error "You haven't typed any commands yet!"))
	((eq last-command 'mud-browse-input-history)
	 (setq mud-input-index (+ mud-input-index delta)))
	((save-excursion (eq (mud-find-input) (point)))
	 (setq mud-input-index 0))
	(t
	 (ding)
	 (message "Press %s again to erase line."
		  (key-description (this-command-keys)))
	 (setq delta nil)))
  (setq this-command 'mud-browse-input-history)
  (if delta
      (let ((end (mud-find-input)))
	(delete-region (point) end)
	(insert (mud-history-ref mud-input-history mud-input-index)))))
  
(defun mud-match-input-history (delta)
  (message (prin1-to-string last-command))
  (cond ((mud-history-empty-p mud-input-history)
	 (error "You haven't typed any commands yet!"))
	((eq last-command 'mud-match-input-history)
	 (setq mud-input-index (+ mud-input-index delta)))
	(t
	 (setq mud-input-index 0)))
  (setq this-command 'mud-match-input-history)
  (let* ((str (concat "^"
		      (regexp-quote (save-excursion 
				      (buffer-substring (mud-find-input)
							(point))))))
	 (tail (nth 1 mud-input-history))
	 (vec (nthcdr 2 mud-input-history))
	 (size (length vec))
	 (found-match nil))
    (while (not (or (eq mud-input-index 
			(+ mud-input-index (* delta size)))
		    found-match))
      (if (string-match str (mud-history-ref mud-input-history
					     mud-input-index))
	  (progn
	    (setq found-match t)
	    (delete-region (mud-find-input) (point))
	    (insert (mud-history-ref mud-input-history mud-input-index)))
	(setq mud-input-index (+ mud-input-index delta))))
    (if (not found-match)
	(error "No match in input history."))))

(defun mud-previous-matching-command ()
  (interactive)
  (mud-match-input-history -1))

(provide 'j-mud-history)
