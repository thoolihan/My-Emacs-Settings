; eieio
(add-path "ecb-2.40/")
(add-path"eieio/")
(autoload 'eieio "eieio" nil t)

; semantic
;(autoload 'semantic "semantic" nil t)
;(setq semantic-load-turn-everything-on t)
;(require 'semantic-load)

; speedbar
(autoload 'speedbar "speedbar" nil t)
   (define-key-after (lookup-key global-map [menu-bar tools])
      [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])
(global-set-key [(f4)] 'speedbar-get-focus)

; ecb
(require 'ecb)

(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
  t)

(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)

(window-number-mode 1)
(window-number-meta-mode 1)
