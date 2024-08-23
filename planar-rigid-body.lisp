
(defun name-attach-sym (name sym)
  (cond
    ((keywordp sym)
     (read-from-string (format nil ":~a~a" name sym)))
    ((numberp sym) sym)
    (t (read-from-string (format nil "~a~a" name sym)))))

(defun name-the-form (name sym-list target)
  (let* ((named-list (mapcar #'(lambda (x) (name-attach-sym name x)) sym-list))
         (sym-name-pair (mapcar #'cons sym-list named-list)))
    (sublis sym-name-pair target)))

(defun extract-sym-list (state)
  (reduce #'(lambda (x y) (if (listp y) (append x y) (cons y x))) state))

(defun collect-keys-values (plst)
  (values (loop for (key value) on plst by #'cddr collect key)
          (loop for (key value) on plst by #'cddr collect value)))

(defun model-construct (state-form deri-form frame-form)
  `(lambda (para-list name)
     "目前最好给定的name是无空格的，且无/或者|(或许可以用随机生成id代替);"
     (let* ((state-form ',state-form)
            (sym-list (extract-sym-list state-form))
            (deri-form ',deri-form)
            (frame-form ',frame-form))
       (multiple-value-bind (para-keys para-values)
           (collect-keys-values para-list)
         (let* ((para-key-names
                  (mapcar #'(lambda (x) (read-from-string (format nil "~a" x)))
                          para-keys))
                (sym-value-pair (mapcar #'cons para-key-names para-values)))
           (let ((named-state (name-the-form name sym-list state-form))
                 (named-deri (sublis sym-value-pair
                                     (name-the-form name sym-list deri-form)))
                 (named-frame (sublis sym-value-pair
                                      (name-the-form name sym-list frame-form))))
             (values named-state named-deri named-frame)))))))

(let ((func-form
       (model-construct
        '((theta d-theta) (omega d-omega) alpha aOX aOy FAx FAy Ax Ay)
        '((d-theta (omega) omega) (d-omega (alpha) alpha))
        '((FAx () 0) (FAy () 0) (aOx () 0) (aOy () 0)
          (alpha (theta FAx FAy aOx aOy)
           (/ (+ (* -1 (+ 9.8 aOy) (sin theta))
               (* -1 aOx (cos theta))
               (* k (/ FAx m) (cos theta))
               (* k (/ FAy m) (sin theta)))
            l))
          (Ax (theta) (* k l (sin theta)))
          (Ay (theta) (* -1 k l (cos theta)))))))
  (defun hinge-rod-model (para-list name)
    (funcall (eval func-form) para-list name))
  func-form)

(let* ((name "simple-rod"))
  (multiple-value-bind (state deri frame-inner)
      (hinge-rod-model '(:m 1.0 :l 0.25 :k 1.0) name)
    (let* ((solver (solver-constructor:solver-create state deri frame-inner))
           (eval-solver (eval solver))
           (initial-cond '(:theta 0.3 :alpha 0 :omega 0 :aOx 0 :aOy 0 :FAx 0 :FAy 0))
           (initial-cond (mapcar #'(lambda (x) (name-attach-sym name x)) initial-cond))
           (result (funcall eval-solver initial-cond 0.01 100)))
      (dump-result t result))))

(defun hinge-rod-extract (name result)
  (let* ((access-sym-list '(:Ax :Ay))
         (named-sym-list (mapcar #'(lambda (x) (name-attach-sym name x))
                                 access-sym-list))
         (name-sym-pair (mapcar #'cons named-sym-list access-sym-list))
         (extrated-result (sublis name-sym-pair result)))
    extrated-result))

(defun hinge-rod-convert (name result)
  (let ((result (hinge-rod-extract name result)))
    (mapcar #'(lambda (frame)
                `((:|line|
                    (:|x1| 0 :|y1| 0
                     :|x2| ,(or (getf frame :Ax) 0)
                     :|y2| ,(- (or (getf frame :Ay) 0))))))
            result)))

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
