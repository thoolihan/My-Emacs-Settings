(add-path "shell/")

 (require 'shell-completion)

(defun git-bash ()
  "Run git-bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name my-gitbash-path)
        (explicit-sh-args '("-login" "-i")))
    (call-interactively 'shell)))

(setq eshell-save-history-on-exit t)
(setq eshell-ask-to-save-last-dir 'always)