;;;; 本脚本用于实施微分方程数值求解

(defun dump-result (stream result)
  (format stream "~{~a~%~}" result))

;;; 此为示例
(defun demo-1 (stream)
  (let ((state-form-def '(x (y dy) (z dz)))
        (derivative-form-def '((dy (x) x) (dz (y) (- y))))
        (frame-inner-form-def '((x (y z) (+ (* 0.1 y) z)))))
    (let* ((solver
             (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
           (solver-eval (eval solver))
           (result (funcall solver-eval '(:x 0 :y 0 :z 1) 0.1 100)))
      (dump-result stream result))))

;;; 此为另一个示例
;; dx/dt = -y
;; dy/dt = x
(defun demo-2 (stream)
  (let ((state-form-def '((x dx) (y dy)))
        (derivative-form-def '((dx (y) (- y)) (dy (x) x)))
        (frame-inner-form-def '()))
    (let* ((solver
             (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
           (solver-eval (eval solver))
           (result (funcall solver-eval '(:x 0 :y 0.2) 0.01 100)))
      (dump-result stream result))))

;;; 此为摆锤
;; 1. 参数
;;    1) k 为重心占总长的比例
;;    2) [x] m 为摆锤的质量
;;    3) l 为摆锤的长度
;;    4) g 重力加速度
;; 2. 状态量
;;    1) theta: 摆锤的角度
;;    2) alpha: 摆锤的角加速度
;;    3) omega: 摆锤的角速度
;; 3. 输出量
;;    1) 位置: x, y为自由端的坐标; 固定端坐标为0, 0
;;    2) 速度: vx, vy为自由端速度
;;    3) 加速度: ax, ay为自由端加速度
;; 公式
;; - g sin(theta) = alpha l
;; 

(defun demo-pendulum (stream)
  "单摆的计算"
  (let ((state-form-def '((theta d-theta) (omega d-omega) alpha))
        (derivative-form-def '((d-theta (omega) omega)
                               (d-omega (alpha) alpha)))
        (frame-inner-form-def '((alpha (theta) (/ (* -9.8 (sin theta)) 1.0)))))
    (let* ((solver
             (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
           (solver-eval (eval solver))
           (result (funcall solver-eval '(:theta 0.3 :alpha 0 :omega 0) 0.01 200)))
      (dump-result stream result))))

(defun demo-pendulum* (&optional (stream nil))
  (declare (ignorable stream))
  "计算单摆，考虑接口传递数据(输出量不定义也可正常运行)"
  (let ((m 1.0)
        (l 0.25)
        (k 1.0))
    (let ((state '((theta d-theta) (omega d-omega) alpha
                   aOx aOy ; 输入量
                   FAx FAy ; 输入量
                   Ax Ay)) ; 输出量
          (derivative '((d-theta (omega) omega)
                        (d-omega (alpha) alpha)))
          (frame-inner `((FAx () 0) (FAy () 0) (aOx () 0) (aOy () 0)
                         (alpha (theta FAx FAy aOx aOy) ; 角动量方程
                                (/ (+ (* -1 (+ 9.8 aOy) (sin theta))
                                      (* -1 aOx (cos theta))
                                      (* ,k (/ FAx ,m) (cos theta))
                                      (* ,k (/ FAy ,m) (sin theta)))
                                   ,l))
                         (Ax (theta) (* ,k ,l (sin theta)))
                         (Ay (theta) (* -1 ,k ,l (cos theta))))))
      (let* ((solver
               (solver-constructor:solver-create state derivative frame-inner))
             (eval-solver (eval solver))
             (result (funcall eval-solver '(:theta 0.3 :alpha 0 :omega 0 :aOx 0 :aOy 0 :FAx 0 :FAy 0) 0.01 100)))
        ;(dump-result stream result)
        result))))

(defun demo-pendulum-1 (name)
  (multiple-value-bind (state deri frame-inner)
      (hinge-rod-model '(:m 1.0 :l 0.25 :k 1.0) name)
    (let* ((solver (solver-constructor:solver-create state deri frame-inner))
           (eval-solver (eval solver))
           (initial-cond '(:theta 0.3 :alpha 0 :omega 0 :aOx 0 :aOy 0 :FAx 0 :FAy 0))
           (initial-cond (mapcar #'(lambda (x) (name-attach-sym name x)) initial-cond))
           (result (funcall eval-solver initial-cond 0.01 100)))
      result)))

;(defun init-fun ()
;  (demo-1 nil) (demo-2 nil) (demo-pendulum nil) (demo-pendulum* t) nil)
