;; set active window opacity 1.0, inactive window opacity 0.9, and
;; everytime a different window becomes active, take 200ms to
;; gradually change opacity.
(transitions 200 1 0.9)


(configure-outputs
 :laptop-scale 1.25
 :laptop-resolution "1920x1080"
 :monitor-scale 1.25
 :monitor-resolution "3840x2160")




(defun configure-outputs (&rest args-plist)
  (let* ((laptop-scale (plist-get args-plist :laptop-scale))
         (laptop-resolution (plist-get args-plist :laptop-resolution))
         (monitor-scale (plist-get args-plist :monitor-scale))
         (monitor-resolution (plist-get args-plist :monitor-resolution))

         (outputs (get-outputs))
         (laptop-monitor (find-monitor-by-name "eDP-1" outputs))
         (4k-monitor (find-monitor-by-model "LG 4K" outputs))
         (laptop-active (plist-get laptop-monitor :active))
         (4k-active (plist-get 4k-monitor :active)))

    (when-let ((laptop-active)
               (_ (not 4k-active))
               (laptop-name (plist-get laptop-monitor :name)))
      (print "Only laptop display is active.")
      (set-output :name laptop-name
                  :scale laptop-scale
                  :resolution laptop-resolution))

    (when-let ((4k-active)
               (_ (not laptop-active))
               (4k-name (plist-get 4k-monitor :name)))
      (print "Only external display is active.")
      (set-output :name 4k-name
                  :scale monitor-scale
                  :resolution monitor-resolution))

    ;; when both monitors are connected
    (when-let ((4k-active)
               (laptop-active)
               (4k-name (plist-get 4k-monitor :name))
               (laptop-name (plist-get laptop-monitor :name)))
      (print "Both monitors are active.")
      (setq laptop-monitor
            (set-output :name laptop-name
                        :scale laptop-scale
                        :resolution laptop-resolution))
      (setq 4k-monitor
            (set-output :name 4k-name
                        :scale monitor-scale
                        :resolution monitor-resolution))

      ;; If using home monitor, set 4k-monitor to the left of
      ;; the laptop-monitor.  Else, set 4k-monitor to the right of
      ;; the laptop-monitor
      (if (equal "SERIAL-NUMBER" (plist-get 4k-monitor :serial))
          (position-displays 4k-monitor laptop-monitor 'center)
          (position-displays laptop-monitor 4k-monitor 'bottom)))))


(defun position-displays (left right &optional align)
  (let ((left-name (plist-get left :name))
        (right-name (plist-get right :name))
        (left-width (plist-get left :width))
        (left-height (plist-get left :height))
        (right-width (plist-get right :width))
        (right-height (plist-get right :height)))
    (when (or (eq align 'top) (null align))
      (set-output :name left-name
                  :pos-x 0
                  :pos-y 0)

      (set-output :name right-name
                  :pos-x left-width
                  :pos-y 0))

    (when (eq align 'bottom)
      (set-output :name left-name
                  :pos-x 0
                  :pos-y (if (>= left-height right-height)
                             0
                             (- right-height left-height)))

      (set-output :name right-name
                  :pos-x left-width
                  :pos-y (if (<= left-height right-height)
                             0
                             (- left-height right-height))))

    (when (eq align 'center)
      (set-output :name left-name
                  :pos-x 0
                  :pos-y (if (>= left-height right-height)
                             0
                             (/ (- right-height left-height) 2)))

      (set-output :name right-name
                  :pos-x left-width
                  :pos-y (if (<= left-height right-height)
                             0
                             (/ (- left-height right-height) 2))))))

(defun find-monitor-by-name (name outputs)
  (seq-find
   (lambda (output)
     (and (consp output)
          (equal name (plist-get output :name))
          output))
   outputs))

(defun find-monitor-by-model (model outputs)
  (seq-find
   (lambda (output)
     (and (consp output)
          (equal model (plist-get output :model))
          output))
   outputs))
