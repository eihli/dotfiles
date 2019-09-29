(defun tidy-html ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "tidy -xml -i -q"
   nil
   t))
