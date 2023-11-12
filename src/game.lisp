(in-package #:mad-scientist)

(s:defsketch game-window ((s:title "Mad Scientist")
                          (game (make-game))
                          (clock (sc:make-clock)))
  (let ((*game* game)
        (*game-clock* clock)
        (*game-window* s::*sketch*))
    (draw-game s:width s:height)))

(s:define-start-function (start) game-window (:resizable t :width 800 :height 1000)
  (:start (music-init))
  (:setup (_) (play-soundtrack))
  (:on-close (_) (mute-soundtrack))
  (:quit (music-quit)))

(defmethod kit.sdl2:mousebutton-event :around ((window game-window) state ts button x y)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((window game-window) state ts rep? keysym)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window))
    (call-next-method)))
