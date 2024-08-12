;;;; 本脚本用于实施微分方程数值求解

;;; 此为示例
(defun demo-1 ()
  (let ((state-form-def '(x (y dy) (z dz)))
        (derivative-form-def '((dy (x) /x-1/) (dz (y) (- /y-1/))))
        (frame-inner-form-def '((x (y z) (+ (* 0.1 y) z)))))
    (let* ((solver
             (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
           (solver (eval solver))
           (result (funcall solver '(:x 0 :y 0 :z 1) 0.1 100)))
      result
      (format t "~{~a~%~}" result))))

;;; 此为另一个示例
;; dx/dt = -y
;; dy/dt = x
(defun demo-2 ()
  (let ((state-form-def '((x dx) (y dy)))
        (derivative-form-def '((dx (y) (- /y-1/)) (dy (x) /x-1/)))
        (frame-inner-form-def '()))
    (let* ((solver
             (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
           (solver (eval solver))
           (result (funcall solver '(:x 0 :y 0.2) 0.01 100)))
      (format t "~{~a~%~}" result))))

(defun init-fun ()
  (demo-2))

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

(defun demo-pendulum ()
  "单摆的计算"
  (let ((state-form-def '((theta d-theta) (omega d-omega) alpha))
        (derivative-form-def '((d-theta (omega) /omega-1/)
                               (d-omega (alpha) /alpha-1/)))
        (frame-inner-form-def '((alpha (theta) (/ (* -9.8 (sin theta)) 1.0)))))
    (let* ((solver
             (solver-constructor:solver-create state-form-def derivative-form-def frame-inner-form-def))
           (solver (eval solver))
           (result (funcall solver '(:theta 0.3 :alpha 0 :omega 0) 0.01 200)))
      (format t "~{~a~%~}" result))))

(defun demo-pendulum* ()
  "计算单摆，考虑接口传递数据(输出量不定义也可正常运行)"
  (let ((state '((theta d-theta) (omega d-omega) alpha
                 aOx aOy ; 输入量
                 FAx FAy ; 输入量
                 Ax Ay)) ; 输出量
        (derivative '((d-theta (omega) /omega-1/)
                      (d-omega (alpha) /alpha-1/)))
        (frame-inner '((FAx () 1) (FAy () 1) (aOx () 0) (aOy () 0)
                       (alpha (theta) ; 角动量方程
                        (+ (/ (* (+ aOy -9.8) (sin theta)) 1.0)
                         (/ (* aOx (cos theta)) 1.0)
                         (/ FAx (cos theta))
                         (/ FAy (sin theta))))
                       (Ax (theta) (* 1.0 (sin theta)))
                       (Ay (theta) (* -1.0 (cos theta))))))
    (let* ((solver
             (solver-constructor:solver-create state derivative frame-inner))
           (eval-solver (eval solver))
           (result (funcall eval-solver '(:theta 0.3 :alpha 0 :omega 0 :aOx 0 :aOy 0 :FAx 0 :FAy 0) 0.01 200)))
      (format t "~{~a~%~}" result))))
