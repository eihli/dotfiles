(defpackage #:ow/displays
  (:use :cl)
  (:import-from :stumpwm :run-shell-command)
  (:export #:multi--mon))

(in-package #:ow/displays)

(defun multi--mon ()
  (run-shell-command
   (concatenate 'string
                "xrandr"
                " --output HDMI-1-0"
                " --primary"
                " --mode 1920x1080"
                " --pos 0x840"
                " --same-as eDP-1"
                " --output eDP-1"
                " --mode 1920x1080"
                " --output DP-1-3"
                " --rotate left"
                " --left-of HDMI-1-0"
                " --auto") t)
  (run-shell-command
   "xrandr --output DP-1-3 --pos 0x-840"))
