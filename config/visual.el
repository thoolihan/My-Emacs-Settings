; look and feel
(add-path "color-theme/")
(add-path "color-theme/themes/")

(when 
    (or 
     (eq my-ostype 'windows) 
     (eq my-ostype 'mac) 
     (eq window-system 'x))
  (setq frame-title-format "edit - %b")
  (require 'color-theme)
  (color-theme-initialize)
  (eval `(,my-theme)))

(set-face-font 'default my-type-face)

(global-hl-line-mode t)

(add-path "misc/")
(if (eq my-use-col-highlight t)
    (do (require 'col-highlight)
        (column-highlight-mode t)))

