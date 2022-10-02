(displays
 ;; from left to right
 `(DP-3
  :scale 1.0
  :res "3840x2160")
 `(HDMI-1
   :scale 1.2
   :res "1920x1080"))

;; set active window opacity 1.0, inactive window opacity 0.9, and
;; everytime a different window becomes active, take 200ms to
;; gradually change opacity.
(transitions 200 1 0.9)
