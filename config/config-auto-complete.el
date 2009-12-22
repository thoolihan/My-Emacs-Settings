(require 'auto-complete)
(global-auto-complete-mode t)
; allow C-n C-p to move in completion list
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
; complete with tabs
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map " " 'ac-complete)

