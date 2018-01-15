;;;; tile.asd

(asdf:defsystem #:tile
  :description "Describe tile here"
  :author "Francis St-Amour"
  :license "BSD 2-clause \"Simplified\" License"
  :depends-on (#:trivial-raw-io
               #:cl-strings
               #:anaphora
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "control-code")
               (:file "buffer")
               (:file "tile")))

