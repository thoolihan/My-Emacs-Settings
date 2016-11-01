; customize editor
(global-linum-mode 1)
(global-auto-revert-mode 1)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (if (eq my-usemenu t)
        (menu-bar-mode 1)
      (menu-bar-mode -1)))

(column-number-mode t)
(setq default-directory my-workspace-dir)
(setq desktop-path (list emacs-root))
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

; also see config-style
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq split-width-threshold nil)  ;fixes window behavior
(global-set-key [f12] 'toggle-truncate-lines)

(setq truncate-partial-width-windows nil)
(setq-default truncate-lines 'true)
(defalias 'yes-or-no-p 'y-or-n-p)
(display-time)
;(setq-default transient-mark-mode nil)
(delete-selection-mode t)
(add-hook 'c-mode-common-hook
               (lambda () (subword-mode t)))

; enable cua (copy & paste), but disable key bindings
;  this allows for rectangular mark with C-<Enter>
(cua-mode)
(setq cua-enable-cua-keys nil)
(subword-mode t)

(setq vc-handled-backends `(Git))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

; emacs options
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(browse-url-browser-function (quote browse-url-default-windows-browser))
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

(setq c-default-style "k&r")
(define-key text-mode-map (kbd "<tab>") 'tab-to-tab-stop)
