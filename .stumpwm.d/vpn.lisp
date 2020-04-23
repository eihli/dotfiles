(defpackage :ow/stumpwm-vpn
  (:use
   :common-lisp
   :cl-ppcre
   :uiop)
  (:export :vpn-mode-line-string))

(in-package :ow/stumpwm-vpn)

(defun vpn-line-match (line)
  (ppcre:register-groups-bind (ip) ("\s*([\\d\.]+) .* tun0" line)
    ip))

(defun network-info ()
  (uiop:run-program "ip a s" :output :lines))

(defun pipe (value &rest fns)
  (let ((fn (car fns)))
    (if (or (null value) (null fn))
        value
        (apply #'pipe (funcall fn value) (cdr fns)))))

(defun ow/vpn ()
  (nth 5
       (ppcre:split
        "\\s"
        (car
         (remove-if-not
          #'vpn-line-match
          (network-info))))))

(defun vpn-mode-line-string ()
  (let ((vpn (ow/vpn)))
    (if vpn
        (concatenate 'string "^[^2 " vpn "^]")
        "^[^1 VPN^]")))
