(load-file "~/emacs/nxhtml/autostart.el")

(setq auto-mode-alist
      (append '(("\\.xaml$" . nxhtml-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.config$" . nxhtml-mode)) auto-mode-alist))