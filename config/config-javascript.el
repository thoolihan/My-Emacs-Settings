; There are two javascript shell modes loaded.  The first is Steve Yegge's
; which he refers to as a toy The second runs rhino (javascript vm for jvm)
; js2-mode - for syntax highlighting
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
               (lambda () (c-subword-mode 1)))
; ej from yegge, -- console
(autoload 'js-console "js-console" nil t)
; javascript-mode - for running shell in a buffer
(load-library "javascript-mode")
(setq javascript-shell-command "java")
(setq javascript-shell-command-args (list "-jar" "c:/temp/js.jar"))
