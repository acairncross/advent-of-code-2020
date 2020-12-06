(require "split-sequence")
(require "fset")

(defun group->question-set (group initial-set combine)
  (loop
    for person in group
    with questions = initial-set
    do (setq questions (funcall combine questions (fset:convert 'fset:set person)))
    finally (return questions)))

(defun solve (is variant)
  (let* ((initial-set
           (getf
            (list
             'a (fset:empty-set)
             'b (fset:convert 'fset:set "abcdefghijklmnopqrstuvwxyz")) variant))
         (combine (getf (list 'a #'fset:union 'b #'fset:intersection) variant))
         
         (lines (loop
                 for line = (read-line is nil)
                 while line
                 collect line))
         (groups (split-sequence:split-sequence "" lines :test #'string=))
         (question-sets
           (mapcar (lambda (g) (group->question-set g initial-set combine)) groups)))
    (loop
      for question-set in question-sets
      sum (fset:size question-set))))

(defun solve-a (is) (solve is 'a))

(defun solve-b (is) (solve is 'b))
