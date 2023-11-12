(in-package #:mad-scientist)

(defun make-room ()
  (list :map #2a((nil nil  t  nil nil)
                 ( t  nil nil nil nil)
                 ( t  nil nil nil nil)
                 (nil nil nil nil nil))))

(defun make-game ()
  (list :room (make-room)
        :x 0d0
        :y 0d0))

(defun dx (direction)
  (ecase direction
    ((:up :down) 0d0)
    ((:left) -1d0)
    ((:right) 1d0)))

(defun dy (direction)
  (ecase direction
    ((:left :right) 0d0)
    ((:up) -1d0)
    ((:down) 1d0)))

(defun move (direction &optional (speed 1) &aux (map (getf (getf *game* :room) :map)))
  (destructuring-bind (rw rh) (array-dimensions map)
    (let ((x (getf *game* :x))
          (y (getf *game* :y)))
      (incf x (* speed (dx direction)))
      (incf y (* speed (dy direction)))
      (when (and (<= 0 x (1- (* 10 rw)))
                 (<= 0 y (1- (* 10 rh))))
        (let ((x* (floor x 10))
              (y* (floor y 10)))
          (unless (aref map x* y*)
            (setf (getf *game* :x) x
                  (getf *game* :y) y)))))))
