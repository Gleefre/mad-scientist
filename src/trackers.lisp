(in-package #:mad-scientist)

;;; Trackers namespace

(defvar *trackers*)

(defun symbol-tracker (name)
  (gethash name *trackers*))

(defun (setf symbol-tracker) (value name)
  (setf (gethash name *trackers*) value))

(defmacro tracker (name)
  `(symbol-tracker ',name))

(defmacro define-tracker (name initform)
  `(setf (tracker ,name) ,initform))

;;; API for trackers

(defclass tracker () ())

(defgeneric update-tracker (tracker state scancode &key timestamp repeat-p keysym &allow-other-keys))

;;; Default trackers

;; kit.sdl2's tracker
(defclass keystate-tracker (tracker kit.sdl2:keystate-tracker) ())

(defmethod update-tracker ((tracker kit.sdl2:keystate-tracker) state scancode &key repeat-p keysym &allow-other-keys)
  (kit.sdl2:keystate-update tracker state repeat-p keysym))

(defun key-pressed-p (keystate-tracker scancode)
  (kit.sdl2:key-down-p keystate-tracker scancode))

;; "last-pressed" tracker
(defclass last-pressed-tracker (tracker)
  ((scancodes :initform () :initarg :scancodes :accessor lp-tracker-scancodes)
   (%list :initform () :accessor %lp-list)))

(defun make-lp-tracker (&rest scancodes)
  (make-instance 'last-pressed-tracker :scancodes scancodes))

(defmethod update-tracker (tracker state scancode &key repeat-p &allow-other-keys)
  (unless repeat-p
    (when (member scancode (lp-tracker-scancodes tracker))
      (ecase state
        (:keydown (push scancode (%lp-list tracker)))
        (:keyup (a:removef (%lp-list tracker) scancode))))))

(defmethod last-pressed ((tracker last-pressed-tracker))
  (values (car (%lp-list tracker))
          (not (null (%lp-list tracker)))))

;; last-pressed tracker but with values
(defclass last-pressed-map-tracker (last-pressed-tracker)
  ((scancodes-map :initform (make-hash-table) :initarg :map :accessor lp-tracker-map)))

(defun make-lp-map-tracker (scancodes-map)
  (make-instance 'last-pressed-map-tracker
                 :scancodes (a:hash-table-keys scancodes-map)
                 :map scancodes-map))

(defmacro lp-map-tracker (&body scancodes-map-pairs &aux (map (gensym "map")))
  `(let ((,map (make-hash-table)))
     (setf ,@(loop for pair in scancodes-map-pairs
                   for (codes value) = (a:ensure-list pair)
                   for last-value = (if (listp pair) value last-value)
                   append (loop for code in (a:ensure-list codes)
                                collect `(gethash ,code ,map) collect last-value)))
     (make-lp-map-tracker ,map)))

(defmethod last-pressed ((tracker last-pressed-map-tracker))
  (values (gethash (car (%lp-list tracker)) (lp-tracker-map tracker))
          (not (null (%lp-list tracker)))))

;;; Hook into the sketch

(stealth-mixin:define-stealth-mixin sketch-trackers () s:sketch
  ((%trackers :initform (make-hash-table) :accessor %trackers)))

(defmethod s:draw :around ((sketch sketch-trackers) &key &allow-other-keys)
  (let ((*trackers* (%trackers sketch)))
    (call-next-method)))

(defmethod s:setup :around ((sketch sketch-trackers) &key &allow-other-keys)
  (let ((*trackers* (%trackers sketch)))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :before ((window sketch-trackers) state ts rep? keysym)
  (loop for tracker being the hash-value of (%trackers window)
        do (update-tracker tracker state (sdl2:scancode keysym)
                           :timestamp ts :repeat-p rep? :keysym keysym)))
