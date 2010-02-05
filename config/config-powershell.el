(add-path "powershell/")
(require 'powershell-mode)
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))
(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)