#My Emacs Setup

##Install
1. check this out as ~/emacs
2. copy sample.emacs to ~/.emacs
3. copy ~/emacs/local-settings.el.sample to ~/emacs/local-settings.el
4. edit the ~/emacs/local-settings.el file with the options you prefer, example below:

```elisp
;(setq my-ostype 'windows) ; windows mac linux
(setq my-theme 'color-theme-twilight)
;(setq my-theme 'color-theme-charcoal-black)
;(setq my-theme 'color-theme-vim-colors)
;(setq my-theme 'color-theme-feng-shui)
(setq my-type-face "Courier New-13")
(setq my-use-col-highlight nil)
(setq my-startshell t)
(setq my-usemenu t)
(setq use-lambda t)
(setq user-emacs-directory (concat emacs-directory ".emacs.d"))
(setq my-workspace-dir "~/workspace/")
(setq my-dropbox-dir "~/Dropbox")
```

##Questions
twitter: [@thoolihan](http://twitter.com/thoolihan)
