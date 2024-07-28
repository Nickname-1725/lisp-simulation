(asdf:defsystem :lisp-simulation
  :components ((:file "main" :depends-on ("solver-constructor"))
               (:file "solver-constructor")))
