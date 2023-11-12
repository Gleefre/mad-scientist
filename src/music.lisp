(in-package #:mad-scientist)

;;; notes

(defvar *sounds* (make-hash-table :test 'equal))

(defun sound-filename (name)
  (probe-file
   (data-path (format nil "sound/~A.ogg" name))))

(defun sound (name &key (mixer :effect) (volume 1.0))
  (or (gethash name *sounds*)
      (h:create (sound-filename name) :mixer mixer :volume volume)))

(defun (setf sound) (sound name)
  (setf (gethash name *sounds*) sound))

(defparameter *sfx-mute* NIL)

(defun sfx (name)
  (unless *sfx-mute*
    (h:play (sound name) :reset T)))

;; soundtrack

(defun create-soundtrack ()
  (setf (sound :soundtrack)
        (make-instance 'h:environment :sets `((:normal ,(sound-filename "soundtrack"))))))

(defparameter *soundtrack-mute* T)

(defun toggle-soundtrack ()
  (if *soundtrack-mute*
      (play-soundtrack)
      (mute-soundtrack)))

(defun play-soundtrack ()
  (when *soundtrack-mute*
    (h:transition (sound :soundtrack) :normal)
    (setf *soundtrack-mute* NIL)))

(defun mute-soundtrack ()
  (unless *soundtrack-mute*
    (h:transition (sound :soundtrack) NIL)
    (setf *soundtrack-mute* T)))

(defun music-init ()
  (unless h:*server*
    (h:maybe-start-simple-server :mixers '((:music m:basic-mixer) (:effect m:basic-mixer))
                                 :name "Mad Scientist")
    (create-soundtrack)))

(defun music-quit ()
  (when h:*server*
    (h:stop h:*server*)
    (setf h:*server* nil)
    (loop for sound being the hash-value of *sounds*
          do (m:free sound)
          finally (clrhash *sounds*))))
