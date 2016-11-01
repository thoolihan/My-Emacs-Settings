(if window-system
    (progn
      (add-hook 'ediff-before-setup-hook
                'new-frame)

      (add-hook 'ediff-quit-hook
                'delete-frame)

      (add-hook 'ediff-startup-hook
                '(lambda ()
                   (set-frame-size (selected-frame) 120 80)
;                   (set-face-attribute 'default (selected-frame) :height 130)
                   (raise-frame (selected-frame))
                   ))
      ))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
