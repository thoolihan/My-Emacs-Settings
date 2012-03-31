(add-path "yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat emacs-directory "yasnippet/snippets"))