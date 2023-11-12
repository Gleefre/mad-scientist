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

(defparameter +dirs+ '(:scancode-up
                       :scancode-down
                       :scancode-left
                       :scancode-right
                       :scancode-w
                       :scancode-s
                       :scancode-d
                       :scancode-a))

(defun make-direction-tracker ()
  (list :tracker))

(defun update-direction-tracker (tracker state scancode)
  (when (member scancode +dirs+)
    (ecase state
      (:keydown (push scancode (cdr tracker)))
      (:keyup (a:removef (cdr tracker) scancode)))))

(defun last-dir ()
  (case (cadr *direction-tracker*)
    ((:scancode-w :scancode-up) :up)
    ((:scancode-s :scancode-down) :down)
    ((:scancode-a :scancode-left) :left)
    ((:scancode-d :scancode-right) :right)))
