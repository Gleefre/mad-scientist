(in-package #:mad-scientist)

(defun draw-hero (w h)
  (s+:with-fit (10 10 w h)
    (s+:with-fit (100 100 10 10)
      (s+:with-color (s:+black+ :stroke)
        (s:rect 0 0 100 100)))
    (s+:with-color (s:+blue+)
      (s:rect 2 4 6 6))
    (s+:with-color (s:+red+)
      (s:circle 5 2 2))))

(defun draw-game-world (w h &aux (map (getf (getf *game* :room) :map)))
  (destructuring-bind (rw rh) (array-dimensions map)
    (s+:with-fit ((* 10 rw) (* 10 rh) w h)
      (s+:with-color (s:+magenta+ :stroke)
        (s:with-pen (s:make-pen :weight 0.1 :stroke s:+magenta+)
          (s:rect 0 0 (* 10 rw) (* 10 rh))))
      (s:with-translate ((getf *game* :x)
                         (getf *game* :y))
        (draw-hero 2 2))
      (dotimes (x rw)
        (dotimes (y rh)
          (when (aref map x y)
            (s:with-translate ((* 10 x) (* 10 y))
              (s+:with-fit (100 100 10 10)
                (s+:with-color (s:+black+ :stroke)
                  (s:rect 0 0 100 100)))
              (s+:with-color (s:+red+ :stroke)
                (s:line 1 1 9 9)
                (s:line 1 9 9 1)
                (s:line 2 2 2 8)
                (s:line 2 8 8 8)
                (s:line 8 8 8 2)
                (s:line 8 2 2 2)))))))))

(defun draw-game (w h)
  (s:background s:+black+)
  (s+:with-fit (800 800 w h)
    (s:with-pen (s:make-pen :fill (s:hex-to-color "004958"))
      (s:rect 10 60 780 730))
    (s:with-translate (10 60)
      (s:with-pen (s:make-pen)
        (s+:with-scissor (0 0 780 730)
          (draw-game-world 780 730))))
    (s:with-font (s:make-font :size 50 :align :center :color (s:hex-to-color "80DAEB"))
      (s:text (format nil "Mad Scientist") 400 0))))
