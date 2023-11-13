(in-package #:mad-scientist)

;;; Trackers

(defvar *keyboard-tracker*)
(defvar *dir-trackers*)

;; FIXME: move trackers to sketch-utils (maybe),
;; make them ok, not with conses and lists (but with classes & methods instead)
(defun make-direction-tracker (&rest scancodes)
  (cons scancodes (list :tracker)))

(defun make-direction-trackers ()
  (list (make-direction-tracker :scancode-up :scancode-w
                                :scancode-down :scancode-s)
        (make-direction-tracker :scancode-left :scancode-d
                                :scancode-right :scancode-a)))

(defun last-in-direction-tracker (tracker)
  (cadr (cdr tracker)))

(defun update-direction-tracker (tracker state scancode)
  (let ((scancodes (car tracker))
        (llist (cdr tracker)))
    (when (member scancode scancodes)
      (ecase state
        (:keydown (push scancode (cdr llist)))
        (:keyup (a:removef (cdr llist) scancode))))))

(defun last-dir ()
  (case (last-in-direction-tracker (car *dir-trackers*))
    ((:scancode-w :scancode-up)
     (case (last-in-direction-tracker (cadr *dir-trackers*))
       ((:scancode-a :scancode-left) :upl)
       ((:scancode-d :scancode-right) :upr)
       (t :up)))
    ((:scancode-s :scancode-down)
     (case (last-in-direction-tracker (cadr *dir-trackers*))
       ((:scancode-a :scancode-left) :downl)
       ((:scancode-d :scancode-right) :downr)
       (t :down)))
    (t
     (case (last-in-direction-tracker (cadr *dir-trackers*))
       ((:scancode-a :scancode-left) :left)
       ((:scancode-d :scancode-right) :right)))))
