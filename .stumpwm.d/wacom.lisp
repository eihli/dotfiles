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
             (map-to-output (format nil "xsetwacom set ~A MapToOutput 800x900+1920+0" id)))
        (run-shell-command rotate t)
        (run-shell-command map-to-output t)))))

(defcommand do-wacom () ()
  (-do-wacom))
