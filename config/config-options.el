; emacs options
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(browse-url-browser-function (quote browse-url-default-windows-browser))
 `(diary-file (concat emacs-directory "diary"))
 `(ecb-auto-activate t)
 `(ecb-fix-window-size t)
 `(ecb-gzip-setup (quote cons))
 `(ecb-create-layout-file (concat ,emacs-directory "config/.ecb-user-layouts.el"))
 `(ecb-layout-name ,my-ecb-layout)
 `(ecb-options-version "2.40")
 `(ecb-source-path (quote ((,emacs-directory "emacs") (,my-workspace-dir "workspace") ("~" "home") (,my-dropbox-dir "dropbox"))))
 `(ecb-tar-setup (quote cons))
 `(ecb-tip-of-the-day nil)
 `(ecb-tree-buffer-style (quote image))
 `(ecb-tree-indent 1)
 `(ecb-wget-setup (quote cons))
 `(ecb-windows-width 0.15)
 `(frame-title-format 
   (list 
    (getenv "USERNAME") 
    "@" (getenv "HOSTNAME") 
    "(" (getenv "OS") ")"
    " %b %p %m"))
 `(ediff-split-window-function 'split-window-horizontally)
 `(fill-column 78)
 `(inhibit-startup-screen t)
 `(js2-allow-keywords-as-property-names nil)
 `(js2-highlight-level 3)
 `(js2-indent-on-enter-key nil)
 `(ruby-indent-tabs-mode t)
 `(server-kill-new-buffers t))

(setq-default display-buffer-reuse-frames t)
(setq warning-suppress-types nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
