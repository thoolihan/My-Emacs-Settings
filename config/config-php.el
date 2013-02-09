(add-path "php-mode/")

(require 'php-mode)

(add-hook 'php-mode-hook
    '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))