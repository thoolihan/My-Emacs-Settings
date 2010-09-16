(add-path "shell/")

 (require 'shell-completion)

(defun git-bash ()
  "Run git-bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/Program Files (x86)/Git/bin/sh.exe")
        (explicit-sh-args '("-login" "-i")))
    (call-interactively 'shell)))