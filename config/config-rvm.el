(add-path "rvm/")
(require 'rvm)
(rvm-use-default)

(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))
