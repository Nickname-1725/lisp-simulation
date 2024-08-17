
(defparameter *acc* (make-instance 'hunchentoot:easy-acceptor :port 8080))
(setf (hunchentoot:acceptor-document-root *acc*) #P"./www/") ; 前端主页, 使用index.html文件

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (swing-tree :uri "/pendulum"
                                             :default-request-type :post)
    ()
  (let* (;(frame (hunchentoot:parameter "frame")) ; 获取链接中"?var=0"格式的数据
         ;(name (hunchentoot:parameter "name"))
         ;(frame (read-from-string frame))
         ;(json-data (hunchentoot:raw-post-data :force-text t))
         ;(config (jonathan:parse json-data))
         ;(s-t (make-instance 'swing-tree))
         )
    ;(config-class-attr s-t config) ; 根据请求传入的数据配置摇摆树参数
    (let* (;(json-data (jonathan:to-json (swing-tree-animate s-t frame)))
           (result (demo-pendulum*))
           (result (mapcar #'(lambda (frame)
                               `((:|line|
                                   (:|x1| 300 :|y1| 10
                                    :|x2| ,(+ 300 (* 300 (or (getf frame :Ax) 0)))
                                    :|y2| ,(+ 10 (* -300 (or (getf frame :Ay) 0)))))))
                           result))
           (json-data (jonathan:to-json result)))
      ;(dump-json name json-data)
      json-data)))

(defun init-fun ()
  (hunchentoot:start *acc*)
  (loop while t do (sleep 1))) ; 死循环防止可执行文件立刻退出
