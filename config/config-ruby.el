(setq auto-mode-alist
      (append '(("Rakefile$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Gemfile$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Guardfile$" . ruby-mode)) auto-mode-alist))

(setq ruby-indent-level 2)

(add-path "rcodetools/")
(require 'rcodetools)
;(define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
