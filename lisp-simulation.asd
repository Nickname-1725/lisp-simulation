(asdf:defsystem :lisp-simulation
  :components ((:file "main" :depends-on ("solver-constructor"))
               (:file "solver-constructor")
               (:file "web-handler")
               (:file "planar-rigid-body"))
  :depends-on (:hunchentoot
               :jonathan))
