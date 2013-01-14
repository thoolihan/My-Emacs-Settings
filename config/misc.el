(load-library "wc")
(load-library "window-number")
(load-library "countchars.el")
(load-library "google")
(load-library "fullscreen")
(load-library "browse-kill-ring")
(load-library "edit-server")

(if (eq use-lambda t)
    (lambda
      (require 'pretty-lambdada)
      (pretty-lambda-for-modes)))

