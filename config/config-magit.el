(add-path "magit")
(require 'magit)

  (defun magit-escape-for-shell (str)
    (if (or (string= str "git")
        (string-match "^--" str))
          str
        (concat "'" (replace-regexp-in-string "'" "'\\''" str) "'")))