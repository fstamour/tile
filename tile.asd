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

(asdf:defsystem #:tile-test
  :depends-on (#:tile
               #:cl-ppcre
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components
  ((:test-file "tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

