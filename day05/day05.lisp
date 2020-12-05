(defparameter *numeric-form*
  '((#\F . 0) (#\B . 1) (#\L . 0) (#\R . 1)))

(defun split-spec (seat-spec)
  (list (subseq seat-spec 0 7) (subseq seat-spec 7 10)))

(defun spec->bits (spec)
  (reverse
   (loop
     for c across spec
     collect (cdr (assoc c *numeric-form*)))))

(defun spec-bits->id (spec)
  (if spec
      (+ (car spec) (* 2 (spec-bits->id (cdr spec))))
      0))

(defun spec->id (spec)
  (spec-bits->id (spec->bits spec)))

(defun combine-ids (row-id col-id)
  (+ (* 8 row-id) col-id))

(defun seat-spec->seat-id (spec)
  (destructuring-bind (row-spec col-spec) (split-spec spec)
    (combine-ids (spec->id row-spec) (spec->id col-spec))))

(defun find-missing-seat (ids)
  (let* ((copied-ids (copy-seq ids))
         (sorted-ids (sort copied-ids #'<))
         (min-id (car sorted-ids)))
    (loop
      for id in sorted-ids
      for i from 0
      when (/= (- id min-id) i)
        return (+ i min-id))))

(defun solve-a (is)
  (loop
    for line = (read-line is nil)
    while line
    maximize (seat-spec->seat-id line)))

(defun solve-b (is)
  (find-missing-seat
   (loop
     for line = (read-line is nil)
     while line
     collect (seat-spec->seat-id line))))
