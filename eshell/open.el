(defun eshell/op (FILE)
  "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for backslashes"
  (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name FILE))))