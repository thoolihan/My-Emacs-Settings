; clojure
(add-path "clojure-mode/")
(add-path "swank-clojure/")

(require 'slime)
(slime-setup)
(setq swank-clojure-binary "clojure.bat")
(require 'clojure-auto)
(require 'swank-clojure-autoload)
