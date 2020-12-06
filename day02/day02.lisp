(defpackage day02
  (:use cl)
  (:export :solve-a :solve-b))

(in-package :day02)

(defun parse-line (line)
  (multiple-value-bind (string strings)
      (cl-ppcre:scan-to-strings "(\\d+)-(\\d+) ([a-z]): ([a-z]+)" line)
    ;; Can't just not bind to string, so explicitly ignore it to shut up SBCL
    (declare (ignore string))
    strings))

(defun make-db-row (list-row)
  (destructuring-bind (min max target password) list-row
    (list :min (parse-integer min) :max (parse-integer max)
          :target (coerce target 'character) :password password)))

(defun is-valid-a (db-row)
  (let ((cnt (count (getf db-row :target) (getf db-row :password))))
    (and (>= cnt (getf db-row :min)) (<= cnt (getf db-row :max)))))

(defun xor (x y)
  (or (and x (not y)) (and (not x) y)))

(defun is-valid-b (db-row)
  ;; Why does this produce an impenetrable style warning?
  (destructuring-bind (&key min max target password) db-row
    (xor
     (char= target (aref password (- min 1)))
     (char= target (aref password (- max 1))))))

(defun solve-a (is)
  (let ((db (loop
              for line = (read-line is nil)
              while line
              collect (make-db-row (coerce (parse-line line) 'list)))))
    (count-if #'is-valid-a db)))

(defun solve-b (is)
  (let ((db (loop
              for line = (read-line is nil)
              while line
              collect (make-db-row (coerce (parse-line line) 'list)))))
    (count-if #'is-valid-b db)))
