(setq sql-user "epic")
(setq sql-password "pkjamd5nw8")
(setq sql-database "raccd")

(defun epicd () "connect to raccd"
  (interactive)
  (progn
   (setq sql-user "epic")
   (setq sql-password "pkjamd5nw8")
   (setq sql-database "raccd")
   (sql-oracle)))

(defun epicq () "connect to raccq"
  (interactive)
  (progn
   (setq sql-user "epic")
   (setq sql-password "pkjamd5nw8")
   (setq sql-database "raccd")
   (sql-oracle)))

(defun rated () "connect to raccd"
  (interactive)
  (progn
   (setq sql-user "rate_calc")
   (setq sql-password "calc1t;")
   (setq sql-database "isocd1")
   (sql-oracle)))

(defun rateq () "connect to raccd"
  (interactive)
  (progn
   (setq sql-user "rate_calc")
   (setq sql-password "")
   (setq sql-database "isocd2")
   (sql-oracle)))

(defun ratep () "connect to raccd"
  (interactive)
  (progn
   (setq sql-user "rate_calc")
   (setq sql-password "")
   (setq sql-database "isocp1")
   (sql-oracle)))