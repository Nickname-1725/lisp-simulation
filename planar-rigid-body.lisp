
(defun convert (result)
  "对result计算结果进行转换"
  (mapcar #'(lambda (frame)
              `((:|line|
                  (:|x1| 0 :|y1| 0
                   :|x2| ,(or (getf frame :Ax) 0)
                   :|y2| ,(- (or (getf frame :Ay) 0))))))
          result))

(defun scale-trans-ami (scale dx dy ami)
  "对动画进行缩放和平移"
  (let* ((ami
           (mapcar
            #'(lambda (frame)
                (mapcar
                 #'(lambda (elem)
                     (list (car elem)
                           (mapcar #'(lambda (x) (if (numberp x) (* x scale) x))
                                   (cadr elem))))
                 frame))
            ami))
         (ami
           (mapcar
            #'(lambda (frame)
                (mapcar
                 #'(lambda (elem)
                     (let ((attrs (cadr elem)))
                       (setf (getf attrs :|x1|) (+ dx (getf attrs :|x1|)))
                       (setf (getf attrs :|x2|) (+ dx (getf attrs :|x2|)))
                       (setf (getf attrs :|y1|) (+ dy (getf attrs :|y1|)))
                       (setf (getf attrs :|y2|) (+ dy (getf attrs :|y2|)))
                       (list (car elem) attrs)))
                 frame))
            ami)))
    ami))
