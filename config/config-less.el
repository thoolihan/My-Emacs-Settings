(add-path "less/")
(autoload 'less-css-mode "less-css-mode" "Major mode for editing LESS CSS" t)
(setq auto-mode-alist
      (append '(("\\.less$" . less-css-mode)) auto-mode-alist))
