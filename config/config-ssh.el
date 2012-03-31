(setq tramp-default-method "ssh")
(when  
    (not (eq my-ostype 'windows)) 
  (require 'tramp))
(when 
    (eq my-ostype 'windows) 
  (setq default-tramp-method "plink"))