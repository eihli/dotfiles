(defpackage #:ow/stumpwm-audio
  (:use
   :common-lisp
   :stumpwm
   :cl-ppcre)
  (:export #:current-audio-levels))

(in-package #:ow/stumpwm-audio)

(defun current-audio-levels ()
  (let* ((out (run-shell-command "amixer get Master" t))
         (matches (all-matches-as-strings "\\d{2,3}%" out)))
    (format nil " ~{~A~^ ~}" matches)))
