(require "split-sequence")
(require "cl-ppcre")

(defun split-field (field)
  (split-sequence:split-sequence #\: field))

(defun make-passport (fields)
  (let ((passport (make-hash-table :test 'equal)))
    (loop
      for (key value) in (mapcar #'split-field fields)
      do (setf (gethash key passport) value))
    passport))

(defun is-valid-a (passport)
  (= (count-if (lambda (key) (gethash key passport))
                  (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")) 7))

(defun is-number-in-range (value-str lo hi)
  (let ((value (parse-integer value-str)))
    (and (<= lo value) (<= value hi))))

(defun is-valid-height (full-hgt-str)
  (cl-ppcre:register-groups-bind (hgt units)
      ("(\\d+)(cm|in)\\z" full-hgt-str)
    (cond
      ((string= units "cm") (is-number-in-range hgt 150 193))
      ((string= units "in") (is-number-in-range hgt 59 76))
      (t nil))))

(defun is-valid-hair-color (hair-color)
  (cl-ppcre:scan "#[0-9a-f]{6}\\z" hair-color))

(defun is-valid-eye-color (eye-color)
  (cl-ppcre:scan "(amb|blu|brn|gry|grn|hzl|oth)\\z" eye-color))

(defun is-valid-passport-id (passport-id)
  (cl-ppcre:scan "\\A[0-9]{9}\\z" passport-id))

(defun is-valid-b (passport)
  (and (is-valid-a passport)
       (is-number-in-range (gethash "byr" passport) 1920 2002)
       (is-number-in-range (gethash "iyr" passport) 2010 2020)
       (is-number-in-range (gethash "eyr" passport) 2020 2030)
       (is-valid-height (gethash "hgt" passport))
       (is-valid-hair-color (gethash "hcl" passport))
       (is-valid-eye-color (gethash "ecl" passport))
       (is-valid-passport-id (gethash "pid" passport))))

(defun solve-a (is)
  (let* ((passports-txt (loop
                         for passport-txt = (loop
                                              for line = (read-line is nil)
                                              while (and line (/= (length line) 0))
                                              append (split-sequence:split-sequence #\Space line))
                         while passport-txt
                         collect passport-txt))
         (passports (mapcar #'make-passport passports-txt)))
    (count-if #'is-valid-a passports)))

(defun solve-b (is)
  (let* ((passports-txt (loop
                         for passport-txt = (loop
                                              for line = (read-line is nil)
                                              while (and line (/= (length line) 0))
                                              append (split-sequence:split-sequence #\Space line))
                         while passport-txt
                         collect passport-txt))
         (passports (mapcar #'make-passport passports-txt)))
    (count-if #'is-valid-b passports)))
