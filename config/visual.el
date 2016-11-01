; look and feel
(setq-default cursor-type 'bar)
(global-hl-line-mode t)
(set-face-background 'hl-line my-theme-hline)
(set-cursor-color "#2288ff")

(set-face-bold-p 'bold nil)
; disable bold
(unless (eq my-ostype 'linux)
  (set-face-font 'default my-type-face))
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'light :underline nil))
  (face-list))
