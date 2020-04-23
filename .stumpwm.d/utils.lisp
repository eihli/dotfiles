(defmacro ow/time-cached (seconds name args &rest body)
  (let ((next-time (gensym))
        (last-value (gensym)))
    `(let ((,next-time 0)
           (,last-value))
       (defun ,name ,args
         (let ((now (get-universal-time)))
           (if (< now ,next-time)
               ,last-value
               (setf ,next-time (+ now ,seconds)
                     ,last-value (progn ,@body))))))))
