(defpackage #:ow/wacom
  (:use #:stumpwm
        #:common-lisp
        #:cl-ppcre))

(in-package :ow/wacom)
(defvar cur-win (current-window))

(defvar *offset-x* 1920) ;; or 1280?

(defun -do-wacom ()
  (let ((shell-output (run-shell-command "xsetwacom --list devices" t)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "id: (\\d+).*STYLUS" shell-output)
      (let* ((id (aref groups 0))
             (rotate (format nil "xsetwacom set ~A rotate ccw" id))
             ;; Aspect ratio of 800x900 will mean we need to adjust area of Stylus also.
             (map-to-output (format nil "xsetwacom set ~A MapToOutput 800x900+~A+0" id *offset-x*))
             (tablet-width 15200) ;; xsetwacom get `id` Area
             (tablet-height 9500)
             (stylus-area (format nil "xsetwacom set ~A Area 0 0 ~A ~A"
                                  id (round (* tablet-height (/ 8.0 9))) tablet-height)))
        (run-shell-command rotate t)
        (run-shell-command map-to-output t)
        stylus-area))))

(defun -wacom-cur-win ()
  (let ((width (xlib:drawable-width (window-xwin cur-win)))
        (height (xlib:drawable-height (window-xwin cur-win))))
    (let ((shell-output (run-shell-command "xsetwacom --list devices" t)))
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "id: (\\d+).*STYLUS" shell-output)
        (let* ((id (aref groups 0))
               (rotate (format nil "xsetwacom set ~A rotate half" id))
               ;; Aspect ratio of 800x900 will mean we need to adjust area of Stylus also.
               (map-to-output (format nil "xsetwacom set ~A MapToOutput ~Ax~A+~A+0" id width height *offset-x*))
               (tablet-width 15200) ;; xsetwacom get `id` Area
               (tablet-height 9500)
               (stylus-area (format nil "xsetwacom set ~A Area 0 0 ~A ~A"
                                    id (round (* tablet-height (/ 8.0 9))) tablet-height)))
          (run-shell-command rotate t)
          (run-shell-command map-to-output t)
          stylus-area)))))

(defcommand do-wacom () ()
  (-do-wacom))

(defcommand do-wacom-cur-win () ()
  (-wacom-cur-win))
