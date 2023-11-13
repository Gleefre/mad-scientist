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
