(add-path "coffee-mode/")
(autoload 'coffee-mode "coffee-mode" "Major mode for editing Coffeescript" t)
(setq auto-mode-alist
      (append '(("\\.coffee$" . coffee-mode)) auto-mode-alist))
