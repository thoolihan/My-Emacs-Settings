(add-path "cedet")
(add-path "cedet/common/")
(require 'cedet)

(global-ede-mode 1)
(semantic-load-enable-code-helpers)
(global-cedet-m3-minor-mode 1)
(define-key cedet-m3-mode-map "\C-c " 'cedet-m3-menu-kbd)
