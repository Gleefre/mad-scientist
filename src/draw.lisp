(in-package #:mad-scientist)

(defun draw-game (w h)
  (s:background s:+black+)
  (s+:with-fit (800 800 w h)
    (s:with-pen (s:make-pen :fill (s:hex-to-color "004958"))
      (s:rect 20 20 760 760))
    (s:with-font (s:make-font :size 100 :align :center :color (s:hex-to-color "80DAEB"))
      (s:text (format nil "Mad~%Scientist") 400 100))))
