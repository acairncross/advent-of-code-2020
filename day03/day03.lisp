(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun traverse-map (array-map right-step down-step)
  (loop
    for i from 0 by down-step below (array-dimension array-map 0)
    for j = 0 then (mod (+ j right-step) (array-dimension array-map 1))
    count (char= (aref array-map i j) #\#)))

(defun solve-a (is)
  (let* ((list-map (loop
                    for line = (read-line is nil)
                    while line
                    collect line))
         (array-map (list-to-2d-array list-map)))
    (traverse-map array-map 3 1)))

(defun solve-b (is)
  (let* ((list-map (loop
                     for line = (read-line is nil)
                     while line
                     collect line))
         (array-map (list-to-2d-array list-map)))
    (apply #'* (loop
                 for (right-step down-step) in '((1 1) (3 1) (5 1) (7 1) (1 2))
                 collect (traverse-map array-map right-step down-step)))))
