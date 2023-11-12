(in-package #:mad-scientist)

;; data-path to get resource's path
(defparameter *data-location* "res/")

(let ((data-folder nil))
  (defun data-path (relative-path)
    (setf data-folder
          (or data-folder
              (if (member :deploy *features*)
                  (let ((deploy:*data-location* *data-location*))
                    (deploy:data-directory))
                  (asdf:system-relative-pathname "mad-scientist" *data-location*))))
    (uiop:native-namestring
      (merge-pathnames relative-path data-folder))))

(let ((font))
  (defun s::make-default-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path "font/PromptFont.ttf"))
                                :color s:+black+
                                :size 18)))))

(let ((font))
  (defun s::make-error-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path "font/PromptFont.ttf"))
                                :color s:+black+
                                :size 16)))))

(defun pressed? (&rest scancodes)
  (loop for scancode in scancodes
        thereis (kit.sdl2:key-down-p *keyboard-tracker* scancode)))

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
