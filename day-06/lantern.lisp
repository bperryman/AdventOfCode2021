;;;; lantern fish problem
(defun make-population ()
  (make-array 9 :initial-element 0))

(defun add-fish-to-population (population day-cycle &optional (quantity 1))
  (incf (aref population day-cycle) quantity))

(defun population-size (population)
  (loop for fish-per-cycle across population
        sum fish-per-cycle))

(defun process-lantern-file (file)
  (with-open-file (stream file)
    (let ((line (substitute #\space #\, (read-line stream)))
          (population (make-population)))
      (with-open-stream (ss (make-string-input-stream line))
        (do ((entry (read ss nil) (read ss nil)))
            ((null entry) population)
          (add-fish-to-population population entry))))))

(defun rotate-vector (vector)
  (let ((zero-value (aref vector 0)))
    (loop for i from 0 to (- (length vector) 2)
          do
             (setf (aref vector i) (aref vector (1+ i))))
    (setf (aref vector (1- (length vector))) zero-value)))

(defun simulation (input-file days)
  (let ((population (process-lantern-file input-file)))
    (dotimes (day days (population-size population))
      (let ((day-0 (aref population 0)))
        (rotate-vector population)
        (add-fish-to-population population 6 day-0)))))
