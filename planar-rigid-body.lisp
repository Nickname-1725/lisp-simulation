
(defun convert (result)
  "对result计算结果进行转换"
  (mapcar #'(lambda (frame)
              `((:|line|
                  (:|x1| 300 :|y1| 10
                   :|x2| ,(+ 300 (* 300 (or (getf frame :Ax) 0)))
                             :|y2| ,(+ 10 (* -300 (or (getf frame :Ay) 0)))))))
          result))
