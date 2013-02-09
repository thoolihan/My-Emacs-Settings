(add-path "sql/")

;(require 'sql)
(add-to-list 'same-window-buffer-names "*SQL*")
(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename 
               (concat "~/emacs/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

(if (eq my-ostype 'windows) (setq sql-mysql-options '("-C" "-t" "-f" "-n")))
(setq sql-mysql-program my-mysql-path)

(require 'pg) ; postgres
(require 'plsql) ; plsql mode for oracle scripting
