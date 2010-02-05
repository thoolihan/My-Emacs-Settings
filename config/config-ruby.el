; ruby
(add-path "ruby/")
(add-hook 'c-mode-hook 'ruby-style-c-mode)
(add-hook 'c++-mode-hook 'ruby-style-c-mode)
(setq-default c-file-style "ruby")
(autoload 'ri-ruby "ri-ruby")

(autoload 'run-ruby "inf-ruby"
   "Run an inferior Ruby process")
   (autoload 'inf-ruby-keys "inf-ruby"
     "Set local key defs for inf-ruby in ruby-mode")
   (add-hook 'ruby-mode-hook
        '(lambda ()
           (inf-ruby-keys)
   ))

(setq ri-ruby-script "~/ruby/ri-emacs.rb")
(autoload 'ri "ri-ruby.el" nil t)
; 
; You may want to bind the ri command to a key.  For example to bind it to
; F1 in ruby-mode: Method/class completion is also available.
(add-hook 'ruby-mode-hook (lambda ()
                            (local-set-key 'f1 'ri)
                            (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                            (local-set-key 'f4 'ri-ruby-show-args)
                            ))
