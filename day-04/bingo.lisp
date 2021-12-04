
(defun read-bingo-calls (stream)
  "Read in the calls as drawn out of the machine. Returns a list of calls"
  (let ((numbers (substitute #\space #\, (read-line stream))))
	(with-open-stream (stream (make-string-input-stream numbers))
	  (loop for call = (read stream nil) then (read stream nil)
			while call
			collect call))))

(defun read-bingo-card (stream)
  (loop for reads from 1 to 25
		for card-number = (read stream nil) then (read stream)
		when (null card-number)
			 return ()
		collect card-number))

(defclass bingo-card ()
  ((numbers :initarg :numbers :reader numbers)))

(defun create-bingo-card (stream)
  "Read a bingo card from the stream"
  (let ((card-data (read-bingo-card stream)))
	(when card-data
	  (make-instance 'bingo-card
					 :numbers (make-array 25 :initial-contents card-data)))))

(defun restore-all-bingo-cards (stream)
  (loop for card = (create-bingo-card stream) then (create-bingo-card stream)
		when (null card)
		  return cards
		collect card into cards))

(defmethod mark-entry ((card bingo-card) number)
  (with-slots (numbers) card
	(let ((pos (position number numbers)))
	  (when pos
		(setf (aref numbers pos) nil)))))

(defmethod winning-column ((card bingo-card) column)
  (with-slots (numbers) card
	(do* ((index column (+ index 5))
		  (row 0 (1+ row))
		  (marked (null (aref numbers index))
				  (and marked (null (aref numbers index)))))
		((= row 4) marked))))

(defmethod winning-row ((card bingo-card) row)
  (with-slots (numbers) card
	(do* ((index (* 5 row) (1+ index))
		  (column 0 (1+ column))
		  (marked (null (aref numbers index))
				  (and marked (null (aref numbers index)))))
		 ((= column 4) marked))))

;; Yep, this could be optimised.
(defmethod winning-card ((card bingo-card))
  (or (winning-column card 0)
	  (winning-column card 1)
	  (winning-column card 2)
	  (winning-column card 3)
	  (winning-column card 4)
	  (winning-row card 0)
	  (winning-row card 1)
	  (winning-row card 2)
	  (winning-row card 3)
	  (winning-row card 4)))

(defmethod compute-bingo-score ((card bingo-card) call)
  (* call (reduce #'(lambda (acc n)
					  (if (numberp n)
						  (+ acc n)
						  acc))
				  (numbers card)
				  :initial-value 0)))

§(defun play-bingo (file)
  (block winning-exit
	(with-open-file (stream file)
	  (let ((calls (read-bingo-calls stream))
			(cards (restore-all-bingo-cards stream)))
		(dolist (call calls cards)
		  (dolist (card cards)
			(mark-entry card call)
			(when (winning-card card)
			  (return-from winning-exit (compute-bingo-score card call)))))))))


;;;; Part 2
(defun play-card (card calls)
  (if (null calls)
	  ()
	  (progn
		(mark-entry card (first calls))
		(if (winning-card card)
			(compute-bingo-score card (first calls))
			(play-card card (cdr calls))))))

(defun lose-bingo (file)
  (with-open-file (stream file)
	(let ((calls (read-bingo-calls stream))
		  (cards (restore-all-bingo-cards stream)))
	  (do ((remaining-cards cards (remove-if #'winning-card remaining-cards))
		   (remaining-calls calls (cdr remaining-calls)))
		  ((or (null remaining-calls) (>= 1 (length remaining-cards)))
		   (play-card (first remaining-cards) remaining-calls))
		(mapc #'(lambda (card) (mark-entry card (first remaining-calls)))
			  remaining-cards)))))
