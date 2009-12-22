(require 'cl)

(defun open-in-textmate ()
  "open current buffer-file in textmate"
  (interactive)
  (let ((my-file (buffer-file-name)))
   (shell-command (format "mate %s" my-file))))

(defun open-dir-in-textmate (d)
     "open directory in textmate"
     (interactive "sDirectory ")
     (shell-command (format "mate %s" d)))
