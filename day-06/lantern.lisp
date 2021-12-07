;;;; lantern fish problem
(defun process-lantern-file (file)
  (with-open-file (stream file)
    (let ((line (substitute #\space #\, (read-line stream)))
          (fish (make-array 100
                            :adjustable t
                            :fill-pointer 0
                            :element-type 'fixnum
                            :initial-element 0)))
      (with-open-stream (ss (make-string-input-stream line))
        (do ((entry (read ss nil) (read ss nil)))
            ((null entry) fish)
          (vector-push-extend entry fish))))))

(defun vector-push-all (src dest)
  (loop for entry across src
        do (vector-push-extend entry dest)))

(defun simulation (input-file days)
  (let ((population (process-lantern-file input-file)))
    (dotimes (day days (length population))
      (let ((size (length population)))
        (loop for i from 0 below size
              with children = (make-array (floor size 7)
                                          :element-type 'fixnum
                                          :adjustable t
                                          :fill-pointer 0)
              when (< (decf (aref population i)) 0)
                do
                   (setf (aref population i) 6)
                   (vector-push-extend 8 children)
              finally (vector-push-all children population))))))
