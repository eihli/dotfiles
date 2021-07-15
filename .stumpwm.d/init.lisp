(in-package :stumpwm)
(add-to-load-path #p"~/common-lisp/sly/slynk/")
(require :slynk)
(add-to-load-path #p"~/common-lisp/cl-utilities/")
(require :cl-utilities)
(add-to-load-path #p"~/common-lisp/ppath/")
(require :ppath)
(add-to-load-path #p"~/common-lisp/clx-truetype/")
(mode-line)

;; (load-module "stumptray")
;; (stumptray::stumptray)

(defvar ow/init-directory
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwm.d"))))

(defun ow/load (filename)
  (let ((file (merge-pathnames (concat filename ".lisp")
                              ow/init-directory)))
    (load file)))

(ow/load "utils")
(ow/load "vpn")
(ow/load "ow-cpu-mode-line")
(ow/load "ow-audio-mode-line")
(ow/load "wacom")

(setq *mouse-focus-policy* :click)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application helpers
(defcommand rr-appfinder () ()
  (run-or-raise "xfce4-appfinder" '(:class "appfinder")))

(defcommand rr-firefox () ()
  (run-or-raise "firefox" '(:class "firefox")))

(defcommand rr-chrome () ()
  (run-or-raise "google-chrome-stable" '(:class "Google-chrome")))

(defcommand rr-term () ()
  (run-or-raise "terminal" '(:class "URxvt")))

(load-module "swm-gaps")
(load-module "wallpapers")
(load-module "ttf-fonts")
(load-module "screenshot")
(load-module "stump-backlight")

(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "FiraCodeNerdFontCompleteM Nerd Font" :subfamily "Regular" :size 11))

(wallpapers::a-random-wallpaper)

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
    (run-shell-command "xmodmap -e \"clear lock\"")
    ;; Norman sets ralt_switch, which is annoying. -option resets that so I'm back to having
    ;; an alt on both sides of the keyboard.
    (run-shell-command (format nil "setxkbmap -variant ~a -option lv3:ralt_alt; xmodmap -e \"keycode 66 = Escape Escape\" -e \"keycode 9 = Caps_Lock Caps_Lock\"" ow-keyboard-layout))))

(let ((server-running nil))
  (defcommand ow-slynk () ()
    "Toggle the slynk  server on/off"
    (if server-running
        (progn
          (slynk:stop-server 4006)
          (echo-string
           (current-screen)
           "Stopping slynk.")
          (setf server-running nil))
        (progn
          (slynk:create-server :port 4006
                               :style slynk:*communication-style*
                               :dont-close t)
          (echo-string
           (current-screen)
           "Starting slynk . M-x slime-connect RET RET, then (in-package stumpwm)...")
          (setf server-running t)))))

(defcommand ow-battery () ()
  (message
   (run-shell-command "acpi -b -a -t" t)))

(ow/time-cached
 3 ow/ml-cpu ()
 (concat " | " (ow/stumpwm-cpu:cpu-mode-line-string)))

(ow/time-cached
 10 ow/ml-vpn ()
 (concat " | " (ow/stumpwm-vpn:vpn-mode-line-string)))

(ow/time-cached
 1 ow/ml-audio ()
 (concat " | " (ow/stumpwm-audio:current-audio-levels)))

(setf
 *window-format* "%m%n%s%10c|%25t"
 *screen-mode-line-format*
 '("^[^5*%d^]"
   (:eval (ow/ml-cpu))
   (:eval (ow/ml-vpn))
   (:eval (ow/ml-audio))
   " | ^[^2*%n^]"))

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
      ((search "HDMI1 connected" output)
       (run-shell-command
        (concatenate 'string
                     "xrandr"
                     " --output eDP1"
                     " --dpi 132"
                     " --mode 1600x900"
                     " --right-of HDMI1"
                     " --output HDMI1"
                     " --mode 1920x1080") t))
      (t (run-shell-command "xrandr --auto" t)))
    (loop for head in heads
          do (enable-mode-line screen head t))))

(defcommand xrandr-share-external () ()
  (let* ((output (run-shell-command "xrandr" t))
         (screen (car *screen-list*))
         (heads (screen-heads screen)))
    (run-shell-command
     (concatenate 'string
                  "xrandr"
                  " --output eDP1"
                  " --dpi 132"
                  " --mode 1600x900"
                  " --right-of HDMI1"
                  " --output HDMI1"
                  " --mode 1280x720"
                  " --dpi 67")
     t)
    (loop for head in heads
          do (enable-mode-line screen head t))))

(defun current-heads ()
  (screen-heads (car *screen-list*)))

(require :cl-ppcre)

(defun re-match (re text)
  (let ((scanner (cl-ppcre:create-scanner re)))
    (multiple-value-bind (start end reg-starts reg-ends)
        (cl-ppcre:scan scanner text)
      (let* ((registers (map 'list #'list reg-starts reg-ends))
             (matches (map
                       'list
                       (lambda (r)
                         (subseq text (car r) (cadr r)))
                       registers)))
        (if (null start)
            nil
            (values
             (cons
              (subseq text start end)
              matches)
             (subseq text end)))))))

(defun re-all-matches (re text &optional (result (make-array 0 :fill-pointer t :adjustable t)))
  (multiple-value-bind (match rest)
      (re-match re text)
    (if (null (car match))
        result
        (let ()
          (vector-push-extend match result)
          (re-all-matches re rest result)))))

(defun resolutions (output)
  (map
   'list
   (lambda (r)
     (mapcar #'parse-integer (cdr r)))
   (re-all-matches "(\\d{4})x(\\d{3,4})(?:\\+)" output)))

(defun size->dimensions (size)
  (cl-ppcre:all-matches-as-strings "\\d{3,3}" size))

(defun mm->in (x)
  (/ x 25.4))

(defun optimal-dpi ()
  (let* ((output (run-shell-command "xrandr" t))
         (sizes (cl-ppcre:all-matches-as-strings "\\d{3,3}mm x \\d{3,3}mm" output))
         (dimensions (mapcar
                      (lambda (size)
                        (let ((dimensions (size->dimensions size)))
                          (mapcar (compose #'mm->in #'parse-integer) dimensions)))
                      sizes))
         (rezs (mapcar (lambda (r) (apply #'* r)) (resolutions output)))
         (areas (mapcar
                 (lambda (dim)
                   (apply #'* dim))
                 dimensions)))
    (list
     sizes
     (mapcar
      (lambda (x)
        (destructuring-bind (area pixels) x
          (sqrt (/ pixels area))))
      (mapcar #'list areas rezs)))))

; (optimal-dpi)
; => (("310mm x 170mm" "480mm x 270mm") (132.77296 67.73334))

; (defvar ow-display (xlib:open-display "" :display 0 :protocol nil))

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
                          (send-keys "npm run ng -- serve -c dev --proxy-config proxy.conf.json"))))
      (loop for command in commands
            do (run-shell-command (format nil "tmux ~A" command))))))

(defcommand ow-tmux-dev () ()
  (ow--tmux-dev))

(defun ow--wifi ()
  (message (run-shell-command "nmcli dev wifi list" t)))

(defcommand ow-wifi () ()
  (ow--wifi))

(defun ow--brightness (prct)
  (let* ((prct (/ (max 0 (min prct 100)) 100.0))
         (output (run-shell-command "xrandr" t)))
    (multiple-value-bind (_ displays)
        (cl-ppcre:scan-to-strings "(.*)(?: connected)" output)
      (dolist (display (map 'list #'identity displays))
        (run-shell-command (format nil "xrandr --output ~A --brightness ~A" display prct) t)))))

(defcommand ow-brightness (prct) ((:number "Set brightness to percentage: "))
  (ow--brightness prct))

(defun ow--screenshot (filename)
  (run-shell-command (format nil "import ~A" (ppath:expanduser filename))))

(defcommand ow-screenshot (filename) ((:string "Save screenshot as: "))
  (ow--screenshot filename))

(defun select-group-or-create (name cmd)
  (groups)
  (sort-groups (current-screen)
               (gnew-float "Android Studio")
               (let ()
                 (gselect "And")
                 (run-shell-command "/opt/android-studio/bin/studio.sh"))))

(defcommand android-studio () ()
  "Start Android Studio in its own group"
  (select-group-or-create '("Android Studio" :float t)
                          (run-shell-command "~/.local/bin/studio" nil)))

(set-prefix-key (kbd "C-t"))

(define-key *root-map* (kbd "y") "ow-screenshot")
(define-key *root-map* (kbd "T") "toggle-gaps")
(define-key *root-map* (kbd "C-z") "ow-toggle-keyboard")
(define-key *root-map* (kbd "C-v") "ow-battery")
(define-key *root-map* (kbd "V") "xrandr-auto")

(define-key *root-map* (kbd "C-x") "rr-term")
(define-key *root-map* (kbd "C-i") "rr-appfinder")
(define-key *root-map* (kbd "C-f") "rr-firefox")
(define-key *root-map* (kbd "C-o") "rr-chrome")
(define-key *root-map* (kbd "C-Tab") "fullscreen")
;; (define-key *top-map* (kbd "s-Tab") "fnext")
;; (define-key *top-map* (kbd "s-ISO_Left_Tab") "fprev")
;; (undefine-key *top-map* (kbd "s-Tab"))
;; (undefine-key *top-map* (kbd "s-ISO_Left_Tab"))

