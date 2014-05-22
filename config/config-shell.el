(add-path "shell/")

(require 'shell-completion)

(setq eshell-save-history-on-exit t)
(setq eshell-ask-to-save-last-dir 'always)

(add-path "exec-path-from-shell")
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
