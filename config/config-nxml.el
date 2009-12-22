(load-library "rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xaml\\|aspx\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))
(setq nxml-slash-auto-complete-flag t)