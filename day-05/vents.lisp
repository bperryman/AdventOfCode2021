(defstruct line-segment
  x1 y1 x2 y2)

(defun substitute-all (new old-bag string)
  (if (null old-bag)
      string
      (substitute-all new (rest old-bag) (substitute new (first old-bag) string))))

(defun read-vent-data (line-string)
  (let ((processed-line (substitute-all #\space '(#\, #\- #\>) line-string)))
    (with-open-stream (stream (make-string-input-stream processed-line))
      (make-line-segment :x1 (read stream)
                         :y1 (read stream)
                         :x2 (read stream)
                         :y2 (read stream)))))

(defun vertical-line-p (line)
  (= (line-segment-x1 line) (line-segment-x2 line)))

(defun horizontal-line-p (line)
  (= (line-segment-y1 line) (line-segment-y2 line)))

(defun line-of-interest-p (line)
  (or (horizontal-line-p line)
      (vertical-line-p line)))

(defun make-diagram (size)
  (make-array (list size size)
              :element-type 'fixnum
              :initial-element 0))

(defun plot-point (diagram x y)
  (incf (aref diagram x y)))

(defun plot-vertical-line (diagram start end x)
  (loop for y from start to end
        do (plot-point diagram x y)))

(defun plot-horizontal-line (diagram start end y)
  (loop for x from start to end
        do (plot-point diagram x y)))

(defun plot-line (diagram line)
  (let ((x1 (line-segment-x1 line))
        (y1 (line-segment-y1 line))
        (x2 (line-segment-x2 line))
        (y2 (line-segment-y2 line)))
    (cond
      ((vertical-line-p line)
       (plot-vertical-line diagram (min y1 y2) (max y1 y2) x1))
      ((horizontal-line-p line)
       (plot-horizontal-line diagram (min x1 x2) (max x1 x2) y1))
      (t nil))))

(defun count-crossovers (diagram)
  (let ((linear-diagram (make-array (apply #'* (array-dimensions diagram))
                                    :displaced-to diagram
                                    :element-type 'fixnum)))
    (loop for c across linear-diagram
          when (> c 1) count c)))


(defun process-analysis-file (file diagram-size)
  (let ((diagram (make-diagram diagram-size)))
    (with-open-file (stream file)
      (do ((line (read-line stream nil) (read-line stream nil)))
          ((null line) (count-crossovers diagram))
        (let ((vents (read-vent-data line)))
          (when (line-of-interest-p vents)
            (plot-line diagram vents)))))))

;; Part two
;; really just needs to define a new function to plot data
;; and then redefine a couple of functions to use that one
(defun diagonal-line-p (line)
  (let ((x1 (line-segment-x1 line))
        (y1 (line-segment-y1 line))
        (x2 (line-segment-x2 line))
        (y2 (line-segment-y2 line)))
    (= (abs (- x1 x2)) (abs (- y1 y2)))))

(defun plot-diagonal-line (diagram x y delta-x delta-y steps)
  (dotimes (i steps nil)
    (plot-point diagram x y)
    (incf x delta-x)
    (incf y delta-y)))

(defun validate-file (file)
  (with-open-file (stream file)
    (do ((line (read-line stream nil) (read-line stream nil)))
        ((null line) nil)
      (let ((vents (read-vent-data line)))
        (cond
          ((horizontal-line-p vents) (format t "Horizontal ~a~%" vents))
          ((vertical-line-p vents) (format t "Vertical ~a~%" vents))
          ((diagonal-line-p vents) (format t "Diagonal ~a~%" vents))
          (t (format t "UNKNOWN LINE FORMAT ~a~%" vents)))))))

(defun plot-line (diagram line)
  (let ((x1 (line-segment-x1 line))
        (y1 (line-segment-y1 line))
        (x2 (line-segment-x2 line))
        (y2 (line-segment-y2 line)))
    (cond
      ((vertical-line-p line)
       (plot-vertical-line diagram (min y1 y2) (max y1 y2) x1))
      ((horizontal-line-p line)
       (plot-horizontal-line diagram (min x1 x2) (max x1 x2) y1))
      ((diagonal-line-p line)
       (plot-diagonal-line diagram
                           x1 y1
                           (if (> x2 x1) 1 -1)
                           (if (> y2 y1) 1 -1)
                           (1+ (abs (- x1 x2)))))
      (t nil))))

(defun line-of-interest-p (line)
  (or (horizontal-line-p line)
      (vertical-line-p line)
      (diagonal-line-p line)))
