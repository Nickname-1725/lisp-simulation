;;;; 本脚本用于实施微分方程数值求解

;; dx/dt = x
;; frame: (:x 0)

(defun euler-method (initial-frame dt n)
  (labels ((calc (frame-list dt)
             (let* ((|frame-z-1| (car frame-list))
                    (|x(z-1)| (getf |frame-z-1| :x)))
               ; 开始构造新的一帧
               (let ((|frame-z-0| `(:x ,(+ |x(z-1)| (* dt |x(z-1)|)))))
                 |frame-z-0|)))
           (handler (frame-list n)
             (cond
               ((eql n 0) frame-list)
               (t (push (calc frame-list dt) frame-list)
                  (handler frame-list (1- n))))))
    (let (frame-list)
      (push initial-frame frame-list)
      (handler frame-list n))))

;; dz/dt = y
;; dy/dt = x
;; z + y = x

(defun sol-method (initial-frame dt n)
  (labels ((integrator (frame-list d-list dt)
             "积分器，根据导数和之前的值计算现在的值"
             (let ((|frame(z-1)| (car frame-list))
                   (|d(z-1)| (car d-list)))
               (let* ((|y(z-1)| (getf |frame(z-1)| :y))
                      (|z(z-1)| (getf |frame(z-1)| :z))
                      (|y'| (getf |d(z-1)| :y))
                      (|z'| (getf |d(z-1)| :z)))
                 (let ((y (+ (* dt |y'|) |y(z-1)|))
                       (z (+ (* dt |z'|) |z(z-1)|)))
                   (values y z)))))
           (calc (frame-list d-list dt)
             "计算器，进行一帧的计算"
             (let* ((|frame-z-1| (car frame-list))
                    (|x(z-1)| (getf |frame-z-1| :x))
                    (|y(z-1)| (getf |frame-z-1| :y)))
               ;; 计算导数
               (let* ((|y'| |x(z-1)|)
                      (|z'| (- |y(z-1)|)))
                 ;; 积分器进行积分
                 (push `(:y ,|y'| :z ,|z'|) d-list)
                 (multiple-value-bind (y z) (integrator frame-list d-list dt)
                   ;; 进行帧内计算
                   (let ((x (+ (* 0.1 y) z)))
                     ;; 构造新的帧
                     `(:x ,x :y ,y :z ,z))))))
           (handler (frame-list d-list n)
             "管理器，使计算器计算指定帧数"
             (cond
               ((eql n 0) frame-list)
               (t (push (calc frame-list d-list dt) frame-list)
                  (handler frame-list d-list (1- n))))))
    (let (frame-list d-list)
      (push initial-frame frame-list)
      (handler frame-list d-list n))))
