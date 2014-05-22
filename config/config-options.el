; emacs options
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(browse-url-browser-function (quote browse-url-default-windows-browser))
 `(diary-file (concat emacs-directory "diary"))
  `(frame-title-format 
   (list 
    (getenv "USERNAME") 
    "@" (getenv "HOSTNAME") 
    "(" (getenv "OS") ")"
    " %b %p %m"))
 `(ediff-split-window-function 'split-window-horizontally)
 `(fill-column 78)
 `(inhibit-startup-screen t)
 `(server-kill-new-buffers t))

(setq-default display-buffer-reuse-frames t)
(setq warning-suppress-types nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
