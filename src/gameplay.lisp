(in-package #:mad-scientist)

(defun make-room ()
  (list :map #2a((nil nil  t  nil nil)
                 ( t  nil nil nil nil)
                 ( t  nil nil nil nil)
                 (nil nil nil nil nil))))

(defun make-game ()
  (list :room (make-room)
        :x 0
        :y 0))

(defun dx (direction)
  (ecase direction
    ((:up :down) 0)
    ((:left) -1)
    ((:right) 1)))

(defun dy (direction)
  (ecase direction
    ((:left :right) 0)
    ((:up) -1)
    ((:down) 1)))

(define-symbol-macro *map* (getf (getf *game* :room) :map))

(defun on-map? (x y)
  (destructuring-bind (rw rh) (array-dimensions *map*)
    (and (<= 0 x (1- (* 10 rw)))
         (<= 0 y (1- (* 10 rh)))
         (let ((x* (floor x 10))
               (y* (floor y 10)))
           (not (aref *map* x* y*))))))

(defun can-stand? (x y &aux (eps 1/1000))
  (loop for dx in (list eps (- 2 eps))
        always (loop for dy in (list eps (- 2 eps))
                     always (on-map? (+ x dx) (+ y dy)))))

(defun move (direction &optional (speed 1))
  (let ((x (getf *game* :x))
        (y (getf *game* :y))
        (dx (* speed (dx direction)))
        (dy (* speed (dy direction))))
    (when (can-stand? (+ x dx) (+ y dy))
      (incf (getf *game* :x) dx)
      (incf (getf *game* :y) dy))))

(defun move* (&optional (speed 0.1))
  (cond ((pressed? :scancode-up :scancode-w)
         (move :up speed))
        ((pressed? :scancode-down :scancode-s)
         (move :down speed))
        ((pressed? :scancode-left :scancode-a)
         (move :left speed))
        ((pressed? :scancode-right :scancode-d)
         (move :right speed))))
