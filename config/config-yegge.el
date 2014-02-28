; recreate scratch if killed
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ; Since we killed it, don't let caller do that.
  nil)

; make sure scratch is writable
(switch-to-buffer "*scratch*")
(toggle-read-only -1)
(switch-to-buffer (other-buffer))

; split window should show next buffer
(defadvice split-window-vertically
  (after my-window-splitting-advice first () activate)
  (set-window-buffer (next-window) (other-buffer)))

(defadvice split-window-horizontally
    (after my-window-splitting-advice first () activate)
    (set-window-buffer (next-window) (other-buffer)))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(defun swap-buffers ()
  "If you have 2 windows, it swaps them." 
  (interactive) 
  (let* ((w1 (first (window-list)))
         (w2 (second (window-list)))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)))

(defalias 'swap-window 'swap-buffers)
(defalias 'sw 'swap-window)
