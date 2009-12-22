     ;;; rhtml-mode
     (add-to-list 'load-path "~/emacs/rhtml-mode")
     (require 'rhtml-mode)
     (add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

