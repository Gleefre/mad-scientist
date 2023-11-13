(in-package #:mad-scientist)

;;; Vectors math

(declaim (inline x (setf x) y (setf y) vec vec*))

(defun x (dir) (aref dir 0))
(defun (setf x) (value dir) (setf (aref dir 0) value))

(defun y (dir) (aref dir 1))
(defun (setf y) (value dir) (setf (aref dir 1) value))

(defun vec (x y)
  (vector x y))

(defun vec* (x y &aux (norm (sqrt (+ (expt x 2) (expt y 2)))))
  (unless (zerop norm)
    (setf x (/ x norm) y (/ y norm)))
  (vector x y))
