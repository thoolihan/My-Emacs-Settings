; vb.net
(add-path "vbnet-mode/")

(autoload 'vbnet-mode "vbnet-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(vb\\)$" .
                                  vbnet-mode)) auto-mode-alist))
