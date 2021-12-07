(defun process-crab-file (file)
  (with-open-file (stream file)
    (let ((line (substitute #\space #\, (read-line stream)))
          (crabs ()))
      (with-open-stream (ss (make-string-input-stream line))
        (do ((crab-position (read ss nil) (read ss nil)))
            ((null crab-position) (sort crabs #'< :key #'car))
          (if (assoc crab-position crabs)
              (incf (cdr (assoc crab-position crabs)))
              (setf crabs (acons crab-position 1 crabs))))))))

(defun single-move-cost (crab new-location)
  (abs (- new-location (car crab))))

(defun move-costs (crabs new-location single-move)
  (mapcar #'(lambda (position-and-crabs)
              (* (funcall single-move position-and-crabs new-location)
                 (cdr position-and-crabs)))
          crabs))

(defun sum (data)
  (reduce #'+ data))

;; OK, let's brute force this
(defun min-cost (crab-data)
  (let ((min-cost nil))
    (dolist (pos-cost crab-data min-cost)
      (when (or (null min-cost) (< (cdr pos-cost) (cdr min-cost)))
        (setf min-cost pos-cost)))))

(defun analysise-location (file move-calc)
  (let ((crabs (process-crab-file file)))
    (loop for i from (caar crabs) to (caar (last crabs))
          collect (cons i (sum (move-costs crabs i move-calc))) into options
          finally (return (min-cost options)))))

;; part 2
(defun crab-move-cost (crab new-location)
  (let ((cost (single-move-cost crab new-location)))
    (round (* (1+ cost) cost 1/2))))
