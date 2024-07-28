
(load "lisp-simulation.asd")
(asdf:load-system :lisp-simulation)
(ccl:save-application #p"./build/foo-ccl" :toplevel-function #'init-fun
                      :prepend-kernel t)

