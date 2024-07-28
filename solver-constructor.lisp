;;;; 本脚本用于实施微分方程数值求解
(defpackage :solver-constructor
  (:use :cl)
  (:export
   :solver-create))
(in-package :solver-constructor)

(defun integrator-create (state-form-def)
  "积分器生成器"
  (let* ((var-name-pair-list (remove-if-not #'listp state-form-def))
         (var-name-list (mapcar #'car var-name-pair-list))
         (derivative-var-list (mapcar #'cadr var-name-pair-list))
         (last-frame-form
           (mapcar #'(lambda (name) (read-from-string (format nil "/~a-1/" name)))
                   var-name-list))
         (key-word-form
           (mapcar #'(lambda (name) (read-from-string (format nil ":~a" name)))
                   var-name-list))
         (eval-last-frame-var
           (mapcar #'(lambda (last-frame-var key-word)
                       `(,last-frame-var (getf |frame(z-1)| ,key-word)))
                   last-frame-form key-word-form))
         (eval-derivative
           (mapcar #'(lambda (name key-word) `(,name (getf |d(z-1)| ,key-word)))
                   derivative-var-list key-word-form))
         (eval-current-frame-var
           (mapcar #'(lambda (var-name derivative-name last-var-name)
                       `(,var-name (+ (* dt ,derivative-name) ,last-var-name)))
                   var-name-list derivative-var-list last-frame-form)))
    `(integrator (frame-list d-list dt)
                 "积分器，根据导数和之前的值计算现在的值"
                 (let ((|frame(z-1)| (car frame-list)) (|d(z-1)| (car d-list)))
                   (let* (,@eval-last-frame-var ,@eval-derivative)
                     (let (,@eval-current-frame-var)
                       (values ,@var-name-list)))))))

(defun derivative-proto (state-form-def dx)
  "给定state-form-def，给定导数的符号，查询获得原变量的符号"
  (car (find dx state-form-def
             :test #'(lambda (dx item) (if (listp item) (eql dx (cadr item)))))))

(defun calc-create (state-form-def derivative-form-def frame-inner-form-def)
  "计算器生成器"
  (let* (;; 导数计算的依赖
         (derivative-dep-name (mapcar #'caadr derivative-form-def))
         (derivative-dep-full-name
           (mapcar #'(lambda (name) (read-from-string (format nil "/~a-1/" name)))
                   derivative-dep-name))
         (derivative-dep-key-word
           (mapcar #'(lambda (name) (read-from-string (format nil ":~a" name)))
                   derivative-dep-name))
         (derivative-dependency-form
           (mapcar #'(lambda (full-name key-word)
                       `(,full-name (getf |frame(z-1)| ,key-word)))
                   derivative-dep-full-name derivative-dep-key-word))
         ;; 帧间计算
         (derivative-name (mapcar #'car derivative-form-def))
         (derivative-proto (mapcar #'(lambda (dx) (derivative-proto state-form-def dx))
                                   derivative-name))
         (derivative-eval-form
           (mapcar #'(lambda (name item)
                       (let ((derivative-expr (caddr item)))
                         `(,name ,derivative-expr)))
                   derivative-name derivative-form-def))
         (derivative-key-word
           (mapcar #'(lambda (name)
                       (read-from-string (format nil ":~a" name)))
                   derivative-proto))
         (push-d-list-frame-form
           (mapcar #'(lambda (name key-word)
                       `(,key-word ,name))
                   derivative-name derivative-key-word))
         (push-d-list-frame-form
           (cons 'list (reduce (lambda (a b) (append a b)) push-d-list-frame-form)))
         ;; 帧内计算
         (current-frame-name (mapcar #'car frame-inner-form-def))
         (current-frame-expr (mapcar #'caddr frame-inner-form-def))
         (current-frame-eval (mapcar #'(lambda (name expr) `(,name ,expr))
                                     current-frame-name current-frame-expr))
         ;; 此帧构造
         (var-name
           (mapcar #'(lambda (item) (if (listp item) (car item) item)) state-form-def))
         (var-name-key
           (mapcar #'(lambda (name) (read-from-string (format nil ":~a" name)))
                   var-name))
         (frame-construct-form
           (mapcar #'(lambda (key name) `(,key ,name)) var-name-key var-name))
         (frame-construct-form (reduce #'(lambda (x y) (append x y))
                                       frame-construct-form)))
    `(calc (frame-list d-list dt)
           "计算器，进行一帧的计算"
           (let* ((|frame(z-1)| (car frame-list))
                  ,@derivative-dependency-form)
             (let* (,@derivative-eval-form)
               (push ,push-d-list-frame-form d-list)
               (multiple-value-bind ,derivative-proto (integrator frame-list d-list dt)
                 (let (,@current-frame-eval)
                   (list ,@frame-construct-form))))))))

(defun solver-create (state derivative frame-inner)
  "求解器生成器"
  (let ((inte-form (integrator-create state))
        (calc-form (calc-create state derivative frame-inner))
        (handler-form
          `(handler (frame-list d-list n)
                    "管理器，使计算器计算指定帧数"
                    (cond
                      ((eql n 0) frame-list)
                      (t (push (calc frame-list d-list dt) frame-list)
                         (handler frame-list d-list (1- n)))))))
    `(lambda (initial-frame dt n)
       (labels (,inte-form ,calc-form ,handler-form)
         (let (frame-list d-list)
           (push initial-frame frame-list)
           (handler frame-list d-list n))))))