;; set active window opacity 1.0, inactive window opacity 0.9, and
;; everytime a different window becomes active, take 200ms to
;; gradually change opacity.
(transitions 200 1 0.9)


;; configure displays
(displays
 ;; from left to right
 `(DP-3
  :scale 1.0
  :res "3840x2160")
 `(HDMI-1
   :scale 1.2
   :res "1920x1080"))


;; or use named values to call `displays'
(let* (
       (fhd "1920x1080")
       (qhd "3840x2160")

       (laptop-screen
        `(eDP-1))

       (main-screen
        `(HDMI-A-1))

       (main-screen-fhd
        `(HDMI-A-1
          :scale 1.0
          :res ,fhd))

       (main-screen-qhd
        `(HDMI-A-1
          :scale 1.0
          :res ,qhd)))

  (displays main-screen laptop-screen))
