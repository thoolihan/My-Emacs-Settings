; emacs options
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(browse-url-browser-function (quote browse-url-default-windows-browser))
 `(ecb-auto-activate t)
 `(ecb-fix-window-size t)
 `(ecb-gzip-setup (quote cons))
 `(ecb-create-layout-file "~/emacs/config/.ecb-user-layouts.el")
 `(ecb-layout-name ,my-ecb-layout)
 `(ecb-options-version "2.40")
 `(ecb-source-path (quote (("~" "Home") ("~/emacs/" "emacs") (,my-workspace-dir "workspace"))))
 `(ecb-tar-setup (quote cons))
 `(ecb-tip-of-the-day nil)
 `(ecb-tree-buffer-style (quote image))
 `(ecb-tree-indent 1)
 `(ecb-wget-setup (quote cons))
 `(ecb-windows-width 0.15)
 `(fill-column 78)
 `(inhibit-startup-screen t)
 `(js2-allow-keywords-as-property-names nil)
 `(js2-highlight-level 3)
 `(js2-indent-on-enter-key nil)
 `(ruby-indent-tabs-mode t)
 `(server-kill-new-buffers t))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
