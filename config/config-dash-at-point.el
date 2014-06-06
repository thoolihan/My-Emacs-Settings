(add-path "dash-at-point/")
(require 'dash-at-point)
(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)
(add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
(add-to-list 'dash-at-point-mode-alist '(python-mode . "python"))
