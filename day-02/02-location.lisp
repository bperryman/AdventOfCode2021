(defun read-movement (stream)
  (let ((motion (read stream nil))
		(dist (read stream nil)))
	(if (and motion dist)
		(list motion dist)
		())))

(defclass sub ()
  ((distance :initarg :distance :reader distance)
   (depth :initarg :depth :reader depth))
  (:default-initargs :depth 0 :distance 0))

(defmethod reset-sub ((vehicle sub))
  (with-slots (distance depth) vehicle
	(setf distance 0 depth 0)))

(defmethod move-forward ((vehicle sub) dist)
  (with-slots (distance) vehicle
	(incf distance dist)))

(defmethod move-down ((vehicle sub) down)
  (with-slots (depth) vehicle
	(incf depth down)))

(defmethod move-up ((vehicle sub) up)
  (with-slots (depth) vehicle
	(decf depth up)))


(defmethod dispatch-motion ((vehicle sub) movement)
  (let ((action (first movement))
		(distance (second movement)))
	(case action
	  (forward (move-forward vehicle distance))
	  (up (move-up vehicle distance))
	  (down (move-down vehicle distance)))))

(defmethod perform-actions ((vehicle sub) file)
  (with-open-file (actions file :direction :input)
	(do ((action (read-movement actions) (read-movement actions)))
		((null action) (* (depth vehicle) (distance vehicle)))
	  (dispatch-motion vehicle action))))


;;;; part 2 for today

(defclass improved-sub (sub)
  ((aim :initarg :aim :reader aim))
  (:default-initargs :aim 0))

(defmethod reset-sub ((vehicle improved-sub))
  (call-next-method vehicle)
  (with-slots (aim) vehicle
	(setf aim 0)))

(defmethod move-forward ((vehicle improved-sub) dist)
  (with-slots (distance depth aim) vehicle
	(incf depth (* dist aim))
	(incf distance dist)))

(defmethod move-up ((vehicle improved-sub) up)
  (with-slots (aim) vehicle
	(decf aim up)))

(defmethod move-down ((vehicle improved-sub) down)
  (with-slots (aim) vehicle
	(incf aim down)))

(defmethod print-object ((vehicle improved-sub) stream)
  (print-unreadable-object (vehicle stream :type t :identity t)
	(with-slots (distance depth aim) vehicle
	  (format stream "dist: ~d depth: ~d aim: ~d" distance depth aim))))
