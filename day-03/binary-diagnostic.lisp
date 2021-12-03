;;;; part one

(defun process-binary (string)
  (let ((result (make-array (length string) :initial-element 0)))
    (loop for c across string
          for i from 0
          do
             (setf (aref result i) (if (char= c #\0) 0 1)))
    result))

(defun read-binary-from-stream (stream)
  (let ((line (read-line stream nil)))
    (unless (null line)
      (process-binary (string-trim " \n\t" line)))))

(defun add-arrays (vec1 vec2)
  (loop for val across vec2
        for i from 0
        do
           (incf (aref vec1 i) val)))

(defun summarize-binary-file (file)
  (with-open-file (stream file :direction :input)
    (let ((summary (read-binary-from-stream stream)))
      (do ((line (read-binary-from-stream stream)
                 (read-binary-from-stream stream))
           (count 1 (1+ count)))
          ((null line) (values summary count))
        (add-arrays summary line)))))


(defun interpret-vector-as-binary (vect)
  (let ((ans 0))
    (loop for i across vect
          do (setf ans (+ (* ans 2) i)))
    ans))

(defun compute-gamma-epsilon (summary readings)
  "Compute the gamma and epsilon values from the summary and readings values"
  (let ((gamma (interpret-vector-as-binary (map 'vector #'(lambda (x) (if (> x (/ readings 2)) 1 0)) summary))))
    (values gamma (logxor gamma (1- (expt 2 (length summary)))))))

(defun power-consumption (readings-file)
  (multiple-value-bind (summary readings) (summarize-binary-file readings-file)
    (multiple-value-bind (gamma epsilon) (compute-gamma-epsilon summary readings)
      (* gamma epsilon))))

;;;; part two

(defun load-binary-file (file)
  "Load a binary file"
  (with-open-file (stream file :direction :input)
    (loop for entry = (read-binary-from-stream stream)
            then (read-binary-from-stream stream)
          while entry
          collect entry)))

(defun column-summary (data column)
  (let ((result (make-array 2 :initial-element 0)))
    (dolist (datum data (values (aref result 0) (aref result 1)))
      (incf (aref result (aref datum column))))))

(defun keep-only (data column value)
  "Return the data that matches the value in the specified column"
  (remove-if-not #'(lambda (datum) (= (aref datum column) value)) data))

(defun find-by-column (data test &optional (column 0))
  (cond
    ((= 1 (length data)) (first data))
    ((> column (length (first data))) (error "Ran out of columns"))
    (t (multiple-value-bind (zero one) (column-summary data column)
         (let ((keep-value (if (funcall test zero one) 0 1)))
           (find-by-column (keep-only data column keep-value)
                           test
                           (1+ column)))))))

(defun oxygen-co2-summary (report)
  (let ((data (load-binary-file report)))
    (let ((oxygen (find-by-column data #'> 0))
          (co2 (find-by-column data #'<= 0)))
      (* (interpret-vector-as-binary oxygen)
         (interpret-vector-as-binary co2)))))
