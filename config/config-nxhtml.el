(load-file "~/emacs/nxhtml/autostart.el")

(setq auto-mode-alist
      (append '(("\\.xaml$" . nxhtml-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.aspx$" . nxhtml-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.Master$" . nxhtml-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.edml$" . nxhtml-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.config$" . nxhtml-mode)) auto-mode-alist))

(setq mumamo-chunk-coloring 1)