(add-path "rspec")
(require 'rspec-mode)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(add-hook 'ruby-mode-hook 'rspec-minor-mode)

