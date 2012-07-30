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

(set-face-bold-p 'bold nil)
; disable bold
(set-face-font 'default my-type-face)
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list))

(global-hl-line-mode t)
(set-face-background 'hl-line my-theme-hline)

(add-path "misc/")
(if (eq my-use-col-highlight t)
    (do (require 'col-highlight)
        (column-highlight-mode t)))

