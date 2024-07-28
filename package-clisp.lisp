
(load "lisp-simulation.asd")
(asdf:load-system :lisp-simulation)
(ext:saveinitmem #p"./build/foo-clisp" :init-function 
                 (lambda () (init-fun) (ext:quit))
                 :executable t :quiet t :norc t)
