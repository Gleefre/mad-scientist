(in-package #:mad-scientist)

(s:defsketch game-window ((s:title "Mad Scientist")
                          (game (make-game))
                          (clock (sc:make-clock))
                          (%last-time (sc:time clock))
                          (%time-delta 0)
                          (%tracker (make-instance 'kit.sdl2:keystate-tracker))
                          (%dir-trackers (make-direction-trackers)))
  (sc:with-freeze clock
    (setf %time-delta (- (sc:time clock) %last-time)
          %last-time (sc:time clock))
    (let ((*game* game)
          (*game-clock* clock)
          (*game-window* s::*sketch*)
          (*time-delta* %time-delta)
          (*keyboard-tracker* %tracker)
          (*dir-trackers* %dir-trackers))
      (move* 0.2)
      (draw-game s:width s:height))))

(s:define-start-function (start) game-window (:resizable t :width 800 :height 1000)
  (:start (music-init))
  (:setup (_) (play-soundtrack))
  (:on-close (_) (mute-soundtrack))
  (:quit (music-quit)))

(defmethod kit.sdl2:mousebutton-event :around ((window game-window) state ts button x y)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window)
        (*time-delta* (game-window-%time-delta window))
        (*keyboard-tracker* (game-window-%tracker window))
        (*dir-trackers* (game-window-%dir-trackers window)))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((window game-window) state ts rep? keysym)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window)
        (*time-delta* (game-window-%time-delta window))
        (*keyboard-tracker* (game-window-%tracker window))
        (*dir-trackers* (game-window-%dir-trackers window)))
    (kit.sdl2:keystate-update *keyboard-tracker* state rep? keysym)
    (when (not rep?)
      (mapcar (a:rcurry #'update-direction-tracker state (sdl2:scancode keysym))
              *dir-trackers*))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event ((app game-window) state ts rep? keysym))
