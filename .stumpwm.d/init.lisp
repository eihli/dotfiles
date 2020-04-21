(in-package :stumpwm)
(add-to-load-path #p"~/common-lisp/sly/slynk/")
(require :slynk)
(require :cl-utilities)
(require :ppath)
(mode-line)

(defvar ow/init-directory
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwm.d"))))

(defun ow/load (filename)
  (let ((file (merge-pathnames (concat filename ".lisp")
                              ow/init-directory)))
    (load file)))
(ow/load "ow-cpu-mode-line")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpers
;;
;; Lazy streams.
;; Interesting and maybe worth a blog post later.
;; Pulled from SICP.

(defmacro delay (expr)
  `(lambda () ,expr))

(defun force (thunk)
  (if (functionp thunk)
      (funcall thunk)
      thunk))

(defmacro cons-stream (head tail)
  `(cons ,head (delay ,tail)))

(defun head (lazy-stream)
  (car lazy-stream))

(defun tail (lazy-stream)
  (force (cdr lazy-stream)))

(defun cycle (xs &optional cur)
  (cond ((eql cur nil) (cycle xs xs))
        (t (cons-stream (car cur) (cycle xs (cdr cur))))))

(defun range (&optional (start 0) end)
  (cond ((>= start (1- end)) (cons-stream start nil))
        (t (cons-stream start (range (1+ start) end)))))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are the things using "cycle".
;; Not necessary; it's a simple 2-value toggle.
;; But cycle seemed more generic and useful and fun
;; to write.

(let ((ow-keyboard-layout-toggle (cycle '("norman" ",us")))
      (ow-keyboard-layout ",us"))
  (defun ow-toggle-keyboard-layout ()
    (setf ow-keyboard-layout (head ow-keyboard-layout-toggle))
    (setf ow-keyboard-layout-toggle (tail ow-keyboard-layout-toggle)))
  (defcommand ow-toggle-keyboard () ()
    (ow-toggle-keyboard-layout)
    ;; Norman sets ralt_switch, which is annoying. -option resets that so I'm back to having
    ;; an alt on both sides of the keyboard.
    (run-shell-command (format nil "setxkbmap -variant ~a -option" ow-keyboard-layout))))

(let ((server-running nil))
  (defcommand ow-slynk () ()
    "Toggle the slynk  server on/off"
    (if server-running
        (progn
          (slynk:stop-server 4006)
          (echo-string
           (current-screen)
           "Stopping slynk .")
          (setf server-running nil))
        (progn
          (slynk:create-server :port 4006
                               :style slynk:*communication-style*
                               :dont-close t)
          (echo-string
           (current-screen)
           "Starting slynk . M-x slime-connect RET RET, then (in-package stumpwm).")
          (setf server-running t)))))


(defcommand ow-battery () ()
  (message
   (run-shell-command "acpi -b -a -t" t)))


(setf
 *window-format* "%m%n%s%10c|%25t"
 *screen-mode-line-format*
 '("^[^5*%d^]"
   " ^[^2*%n^]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application helpers

(defcommand rr-firefox () ()
            (run-or-raise "firefox" '(:class "firefox")))

(defcommand rr-xterm () ()
  (run-or-raise "xterm" '(:class "XTerm")))


(load-module "swm-gaps")
(load-module "wallpapers")
(wallpapers::a-random-wallpaper)
;; (wallpapers::multiple-wallpapers 0 (* 5 60))

;; Head gaps run along the 4 borders of the monitor(s)
(setf swm-gaps:*head-gaps-size* 0)

;; Inner gaps run along all the 4 borders of a window
(setf swm-gaps:*inner-gaps-size* 10)

;; Outer gaps add more padding to the outermost borders of a window (touching
;; the screen border)
(setf swm-gaps:*outer-gaps-size* 10)

(defcommand xrandr-auto () ()
  (let* ((output (run-shell-command "xrandr" t))
        (screen (car *screen-list*))
        (heads (screen-heads screen)))
    (cond
      ((search "HDMI1 connected" output) (run-shell-command "xrandr --output eDP1 --mode 1600x900 --right-of HDMI1 --output HDMI1 --auto" t))
      (t (run-shell-command "xrandr --auto" t)))
    (loop for head in heads
          do (enable-mode-line screen head t))))


(setf *mouse-focus-policy* :click)

(define-keysym #x1008ff11 "XF86AudioLowerVolume")
(define-keysym #x1008ff12 "XF86AudioMute")
(define-keysym #x1008ff13 "XF86AudioRaiseVolume")

(defcommand |amixer-set-Master-toggle| () ()
  (run-shell-command "amixer set Master toggle -q"))

(defcommand |amixer-set-Master-5%+| () ()
  (run-shell-command "amixer set Master 5%+ -q"))

(defcommand |amixer-set-Master-5%-| () ()
  (run-shell-command "amixer set Master 5%- -q"))

(define-key *top-map* (kbd "XF86AudioMute") "amixer-set-Master-toggle")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-set-Master-5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-set-Master-5%+")

(defun pace-conversion (pace factor)
  (multiple-value-bind (minutes seconds)
      (apply #'values
             (mapcar
              #'parse-integer
              (values (cl-utilities:split-sequence #\: pace))))
    (let* ((total-seconds (+ (* minutes 60) seconds))
           (mi-pace (/ total-seconds factor)))
      (multiple-value-bind (conv-minutes conv-seconds)
          (floor mi-pace 60)
        (apply #'format nil "~A:~2,,,'0@A" (mapcar #'round (list conv-minutes conv-seconds)))))))

(defun pace-km->mi (pace)
  (pace-conversion pace (/ 1 1.609344)))

(defun pace-mi->km (pace)
  (pace-conversion pace 1.609344))

(defcommand km->mi-pace (pace) ((:string "Pace (km): "))
  (pace-km->mi pace))

(defcommand mi->km-pace (pace) ((:string "Pace (mi): "))
  (pace-mi->km pace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts for launching lottery-related dev env.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ow--tmux-dev ()
  (flet ((send-keys (cmd)
           (format nil "send-keys \"~A\" Enter" cmd)))
    (let ((commands (list "new-window"
                          "selectp -t 0"
                          (send-keys "cd ~/code/lottery/")
                          (send-keys "docker-compose run -p 6379:6379 lotto_cache")
                          "splitw -p 50"
                          (send-keys "cd ~/code/lottery/lotto_odds_api/")
                          (send-keys "./run.sh")
                          "splitw -h -p 50"
                          (send-keys "cd ~/code/lottery/scratchoff_odds/")
                          (send-keys "./run.sh")
                          "selectp -t 0"
                          "splitw -h -p 50"
                          (send-keys "cd ~/code/lotto_frontend/")
                          (send-keys "ng serve -c dev --proxy-config proxy.conf.json"))))
      (loop for command in commands
            do (run-shell-command (format nil "tmux ~A" command))))))

(defcommand ow-tmux-dev () ()
  (ow--tmux-dev))

(defun ow--screenshot (filename)
  (run-shell-command (format nil "import ~A" (ppath:expanduser filename))))

(defcommand ow-screenshot (filename) ((:string "Save screenshot as: "))
  (ow--screenshot filename))

(set-prefix-key (kbd "C-t"))

(define-key *root-map* (kbd "y") "ow-screenshot")
(define-key *root-map* (kbd "T") "toggle-gaps")
(define-key *root-map* (kbd "C-z") "ow-toggle-keyboard")
(define-key *root-map* (kbd "C-v") "ow-battery")
(define-key *root-map* (kbd "V") "xrandr-auto")

(define-key *root-map* (kbd "C-x") "rr-xterm")
(define-key *root-map* (kbd "C-f") "rr-firefox")
(define-key *root-map* (kbd "C-Tab") "fullscreen")
;; (define-key *top-map* (kbd "s-Tab") "fnext")
;; (define-key *top-map* (kbd "s-ISO_Left_Tab") "fprev")
;; (undefine-key *top-map* (kbd "s-Tab"))
;; (undefine-key *top-map* (kbd "s-ISO_Left_Tab"))
