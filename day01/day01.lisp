;; Usage: (with-open-file (is "input") (solve-a is))
(defun solve-a (is)
  (let ((entries (loop
                   for line = (read-line is nil)
                   while line
                   collect (parse-integer line))))
    (loop
      for x in entries and i from 0
      nconc (loop
        for y in entries and j from 0
        when (and (< j i) (= (+ x y) 2020))
          collect '(x y)))))

(defun solve-b (is)
  (let ((entries (loop
                   for line = (read-line is nil)
                   while line
                   collect (parse-integer line))))
    (loop
      for x in entries and i from 0
      nconc (loop
              for y in entries and j from 0
              when (< j i)
                nconc (loop
                        for z in entries and k from 0
                        when (< k j)
                          when (= (+ x y z) 2020)
                            collect (* x y z))))))
