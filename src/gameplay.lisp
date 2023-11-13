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

;;; Trackers

(defun init-trackers ()
  (define-tracker :vertical (lp-map-tracker
                              ((:scancode-up :scancode-w) -1)
                              ((:scancode-down :scancode-s) 1)
                              ((nil) 0)))
  (define-tracker :horizontal (lp-map-tracker
                                ((:scancode-left :scancode-a) -1)
                                ((:scancode-right :scancode-d) 1)
                                ((nil) 0))))

;;; Movement

(defun x (dir) (aref dir 0))
(defun y (dir) (aref dir 1))

(defun dx (dir)
  (* (x dir)
     (if (zerop (y dir))
         1
         (sqrt 0.5d0))))

(defun dy (dir)
  (* (y dir)
     (if (zerop (x dir))
         1
         (sqrt 0.5d0))))

(define-symbol-macro *map* (getf (getf *game* :room) :map))

(defun on-map? (x y)
  (destructuring-bind (rw rh) (array-dimensions *map*)
    (and (<= 0 x (* 10 rw))
         (<= 0 y (* 10 rh))
         (let ((x* (floor x 10))
               (y* (floor y 10)))
           (not (aref *map* x* y*))))))

(defun can-stand? (x y &aux (eps 1/1000))
  (loop for dx in (list eps (- 2 eps))
        always (loop for dy in (list eps (- 2 eps))
                     always (on-map? (+ x dx) (+ y dy)))))

(defun move (direction &optional (speed 1))
  ;; FIXME: sliding
  (let ((x (getf *game* :x))
        (y (getf *game* :y))
        (dx (* speed (dx direction)))
        (dy (* speed (dy direction))))
    (when (can-stand? (+ x dx) (+ y dy))
      (incf (getf *game* :x) dx)
      (incf (getf *game* :y) dy))))

(defun last-direction ()
  (vector
   (last-pressed (tracker :horizontal))
   (last-pressed (tracker :vertical))))

(defun move* (&optional (speed 0.1))
  (move (last-direction) speed))
