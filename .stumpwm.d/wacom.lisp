(defpackage #:ow/wacom
  (:use #:stumpwm
        #:common-lisp
        #:cl-ppcre))

(in-package :ow/wacom)

(defun -do-wacom ()
  (let ((shell-output (run-shell-command "xsetwacom --list devices" t)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "id: (\\d+).*STYLUS" shell-output)
      (let* ((id (aref groups 0))
             (rotate (format nil "xsetwacom set ~A rotate ccw" id))
             ;; Aspect ratio of 800x900 will mean we need to adjust area of Stylus also.
             (map-to-output (format nil "xsetwacom set ~A MapToOutput 800x900+1920+0" id))
             (tablet-width 15200) ;; xsetwacom get `id` Area
             (tablet-height 9500)
             (stylus-area (format nil "xsetwacom set ~A Area 0 0 ~A ~A"
                                  id (round (* tablet-height (/ 8.0 9))) tablet-height)))
        (run-shell-command rotate t)
        (run-shell-command map-to-output t)
        stylus-area))))

(defcommand do-wacom () ()
  (-do-wacom))
