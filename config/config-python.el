(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python " buffer-file-name))))
