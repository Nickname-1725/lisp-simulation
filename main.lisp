;;;; 本脚本用于实施微分方程数值求解
(load "solver-constructor.lisp")

;;; 此为示例
(let ((state-form-def '(x (y dy) (z dz)))
      (derivative-form-def '((dy (x) /x-1/) (dz (y) (- /y-1/))))
      (frame-inner-form-def '((x (y z) (+ (* 0.1 y) z)))))
  (let* ((solver
           (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
         (solver (eval solver))
         (result (funcall solver '(:x 0 :y 0 :z 1) 0.1 100)))
    result
    ;(format t "~{~a~%~}" result)
    ))

;;; 此为另一个示例
;; dx/dt = -y
;; dy/dt = x
(let ((state-form-def '((x dx) (y dy)))
      (derivative-form-def '((dx (y) (- /y-1/)) (dy (x) /x-1/)))
      (frame-inner-form-def '()))
  (let* ((solver
           (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
         (solver (eval solver))
         (result (funcall solver '(:x 0 :y 0.2) 0.01 100)))
    (format t "~{~a~%~}" result)))
