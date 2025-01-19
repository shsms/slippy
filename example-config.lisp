;; set active window opacity 1.0, inactive window opacity 0.9, and
;; everytime a different window becomes active, take 200ms to
;; gradually change opacity.
(transitions 200 1 0.9)

;;;;;;;;;;;;;
;; Configs ;;
;;;;;;;;;;;;;

(let* ((outputs (get-outputs))
       (serials (get-monitor-serials outputs))
       (fhd "1920x1080")
       (4k "3840x2160"))
  (cond

    ;;;;;;;;;;;;;;;;;;
    ;; home desktop ;;
    ;;;;;;;;;;;;;;;;;;
    ((and (equal serials '("MONITOR-SERIAL-NUMBER"))
          (equal (length outputs) 1))
        (set-output
         :name (name-from-serial (car serials) outputs)
         :scale 1.38
         :resolution 4k
         :transform "normal"))

    ;;;;;;;;;;;;;;;;;
    ;; home laptop ;;
    ;;;;;;;;;;;;;;;;;
    ((equal serials '("MONITOR-SERIAL-NUMBER"))
     (let ((horiz (name-from-serial (car serials) outputs)))
       (set-output :name "eDP-1" :scale 1.1 :resolution fhd :transform "normal")
       (set-output :name horiz   :scale 1.25 :resolution 4k  :transform "normal")
       ;; usually put the laptop to the right of the monitor at home.
       (order-displays horiz "eDP-1")))

    ;;;;;;;;;;;;;;;;;;
    ;; work desk 12 ;;
    ;;;;;;;;;;;;;;;;;;
    ((equal serials '("MONITOR-SERIAL-NUMBER" "MONITOR-SERIAL-NUMBER"))
     (let ((horiz (name-from-serial (cadr serials) outputs))
           (vert (name-from-serial (car serials)  outputs)))
       (set-output :name "eDP-1" :scale 1.1 :resolution fhd :transform "normal")
       (set-output :name horiz   :scale 1.25 :resolution 4k  :transform "normal")
       (set-output :name vert    :scale 1.25 :resolution 4k  :transform "90")
       ;; usually put the laptop to the left of the monitors at work.
       (order-displays "eDP-1" horiz vert)))

    ;; Unknown desks
    (t (print (format "Unknown displays. Please update the configuration for serials: %s" serials)))))


;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun order-displays (&rest names)
  "Orders displays with given NAMES aligning them at the bottom."
  (let ((outputs (get-outputs))
        (displays (seq-map (lambda (name) (find-monitor-by-name name outputs)) names))
        (heights (seq-map (lambda (display) (plist-get display :height)) displays))
        (max-height (eval `(max ,@heights)))
        (x 0))
    (dolist (display displays)
      (let ((y (- max-height (plist-get display :height))))
        (set-output :name (plist-get display :name)
                    :pos-x x
                    :pos-y y))
      (setq x (+ x (plist-get display :width))))))


(defun find-monitor-by-name (name outputs)
  "Returns the first monitor with given NAME in OUTPUTS."
  (seq-find
   (lambda (output) (equal name (plist-get output :name)))
   outputs))

(defun find-monitor-by-model (model outputs)
  "Returns the first monitor with given MODEL in OUTPUTS."
  (seq-find
   (lambda (output) (equal model (plist-get output :model)))
   outputs))


(defun find-monitor-by-serial (serial outputs)
  "Returns the monitor with given SERIAL in OUTPUTS."
  (seq-find
   (lambda (output) (equal serial (plist-get output :serial)))
   outputs))


(defun name-from-serial (serial outputs)
  "Returns the name of the monitor with given SERIAL in OUTPUTS."
  (plist-get (find-monitor-by-serial serial outputs) :name))


(defun get-monitor-serials (outputs)
  "Returns a list of serials of all monitors in OUTPUTS."
  (sort
   (thread-last outputs
                (seq-filter (lambda (plist) (not (equal "eDP-1" (plist-get plist :name)))))
                (seq-map (lambda (plist) (plist-get plist :serial))))
   (lambda (a b) (string< a b))))
