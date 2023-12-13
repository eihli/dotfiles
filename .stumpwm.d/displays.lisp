(defpackage #:ow/displays
  (:use :cl)
  (:import-from :stumpwm :run-shell-command)
  (:export #:multi--mon))

(in-package #:ow/displays)

(defun multi--mon ()
  (run-shell-command
   (concatenate 'string
                "xrandr"
                " --output DP-3"
                " --scale-from 1920x1080"
                " --output HDMI-0"
                " --primary"
                " --mode 1920x1080"
                " --pos 0x840"
                " --output DP-4"
                " --mode 1920x1080"
                " --rotate normal"
                " --left-of HDMI-0")

 ; => "xrandr --output DP-3 --scale-from 1920x1080 --output HDMI-0 --primary --mode 1920x1080 --pos 0x840 --output DP-4 --mode 1920x1080 --rotate normal --left-of HDMI-0"
   ))

(defun single--mon ()
  (run-shell-command
   (concatenate 'string
                "xrandr"
                " --output eDP-1"
                " --mode 1920x1080")))
