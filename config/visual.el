; look and feel
(when (or (eq my-ostype 'windows) (eq my-ostype 'mac) (eq window-system 'x))
  (setq frame-title-format "edit - %b")
  (require 'color-theme)
  (color-theme-initialize)
  (eval `(,my-theme)))

(when (eq my-ostype 'windows)
  (set-face-font 'default "Monaco-8"))
