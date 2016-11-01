(add-hook 'R-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "R -f " buffer-file-name))))
