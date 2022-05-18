(defpackage #:ow/wacom
  (:use #:stumpwm
        #:common-lisp
        #:cl-ppcre))

(in-package :ow/wacom)
(defparameter cur-win (current-window))

(defvar *offset-x* 1920) ;; or 1280?

(defun -do-wacom ()
  (let ((shell-output (run-shell-command "xsetwacom --list devices" t)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "id: (\\d+).*STYLUS" shell-output)
      (let* ((id (aref groups 0))
             (rotate (format nil "xsetwacom set ~A rotate ccw" id))
             ;; Aspect ratio of 800x900 will mean we need to adjust area of Stylus also.
             (map-to-output (format nil "xsetwacom set ~A MapToOutput 1920x2160+~A+0" id *offset-x*))
             (tablet-width 15200) ;; xsetwacom get `id` Area
             (tablet-height 9500)
             (stylus-area (format nil "xsetwacom set ~A Area 0 0 ~A ~A"
                                  id (round (* tablet-height (/ 8.0 9))) tablet-height)))
        (run-shell-command rotate t)
        (run-shell-command map-to-output t)
        stylus-area))))

(defun current-frame ()
  (stumpwm::tile-group-current-frame (stumpwm:current-group)))

(defun -wacom-cur-frame ()
  "This doesn't robustly take into account modeline.
   The problem is that we get the width/height of the window and map those pixels
   to the tablet using `xsetwacom set ~A MapToOutput XxY`, but we don't have a way to
   automatically detect the X offset (in the case of a vertical line splitting windows)
   nor a way to detect the Y offset (in the case of a mode-line positioned :TOP)."
  (let* ((curframe (current-frame))
         (top (stumpwm:frame-y curframe))
         (left (stumpwm:frame-x curframe))
         (width (stumpwm:frame-width curframe))
         (height (stumpwm:frame-height curframe)))
    (let ((shell-output (run-shell-command "xsetwacom --list devices" t)))
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "id: (\\d+).*STYLUS" shell-output)
        (declare (ignore match))
        (let* ((id (aref groups 0))
               (rotate (format nil "xsetwacom set ~A rotate ccw" id))
               ;; Aspect ratio of 800x900 will mean we need to adjust area of Stylus also.
               (map-to-output (format nil "xsetwacom set ~A MapToOutput ~Ax~A+~A+~A" id width height left top))
               (tablet-width 15200) ;; xsetwacom get `id` Area
               (tablet-height 9500) ;; Unused. Height used to make proportional to screen.
               (stylus-area (format nil "xsetwacom set ~A Area 0 0 ~A ~A"
                                    id (round (* tablet-height (/ 8.0 9))) tablet-height)))
          (declare (ignore tablet-width))
          (run-shell-command rotate t)
          (run-shell-command map-to-output t)
          (run-shell-command stylus-area t))))))

(run-shell-command "xsetwacom set 19 rotate ccw" t)

(defvar *mode-line* (first stumpwm::*mode-lines*))
(xlib:drawable-height (stumpwm::mode-line-window *mode-line*))
(defcommand do-wacom () ()
  (-do-wacom))

(defcommand do-wacom-cur-win () ()
  (-wacom-cur-frame))
