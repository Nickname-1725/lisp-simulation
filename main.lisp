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


