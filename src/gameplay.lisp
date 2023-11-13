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
    ((:up :down) 0d0)
    ((:left) -1d0)
    ((:right) 1d0)
    ((:upl :downl) (- (sqrt 0.5d0)))
    ((:upr :downr) (sqrt 0.5d0))))

(defun dy (direction)
  (ecase direction
    ((:left :right) 0)
    ((:up) -1)
    ((:down) 1)
    ((:upl :upr) (- (sqrt 0.5d0)))
    ((:downl :downr) (sqrt 0.5d0))))

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

(defun move* (&optional (speed 0.1))
  (a:when-let ((direction (last-dir)))
    (move direction speed)))

;;; Trackers

(defun init-trackers ()
  (define-tracker :vertical (make-lp-tracker :scancode-up :scancode-down :scancode-w :scancode-s))
  (define-tracker :horizontal (make-lp-tracker :scancode-left :scancode-right :scancode-a :scancode-d)))

(defun last-dir ()
  (case (last-pressed (tracker :vertical))
    ((:scancode-w :scancode-up)
     (case (last-pressed (tracker :horizontal))
       ((:scancode-a :scancode-left) :upl)
       ((:scancode-d :scancode-right) :upr)
       (t :up)))
    ((:scancode-s :scancode-down)
     (case (last-pressed (tracker :horizontal))
       ((:scancode-a :scancode-left) :downl)
       ((:scancode-d :scancode-right) :downr)
       (t :down)))
    (t
     (case (last-pressed (tracker :horizontal))
       ((:scancode-a :scancode-left) :left)
       ((:scancode-d :scancode-right) :right)))))
