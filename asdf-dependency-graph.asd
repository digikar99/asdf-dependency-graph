(defsystem "asdf-dependency-graph"
  :depends-on ("uiop"
               "optima")
  :license "MIT"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :description "A minimal wrapper around `dot` available at least on Linux systems to generate dependency-graphs."
  :components ((:file "asdf-dependency-graph")))
