; customize editor
(add-path "misc/")
(add-path"slime/")

(require 'linum)
(global-linum-mode)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
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
(setq-default transient-mark-mode t)
(delete-selection-mode t)
(add-hook 'c-mode-common-hook
               (lambda () (c-subword-mode 1)))
; enable cua, but disable key bindings
;  this allows for rectangular mark with C-<Enter>
(cua-mode)
(setq cua-enable-cua-keys nil)
(c-subword-mode t)

(if (eq my-startshell t) 
    (eshell))
;    (if (eq my-ostype 'linux) (term "/bin/bash") (shell)))
(if (eq my-usemenu t) 
    (if (fboundp 'menu-bar-mode) (menu-bar-mode 1)))
