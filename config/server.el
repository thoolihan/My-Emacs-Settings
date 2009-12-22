(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

(custom-set-variables '(server-kill-new-buffers t))
(add-hook 'server-done-hook (lambda () (delete-frame)))