(defsystem "aoc-2020"
  :version "0.0.1"
  :depends-on ("cl-ppcre" "split-sequence" "fset")
  :components
  ((:module "day01"
    :components ((:file "day01")))
   (:module "day02"
    :components ((:file "day02")))
   (:module "day03"
    :components ((:file "day03")))
   (:module "day04"
    :components ((:file "day04")))
   (:module "day05"
    :components ((:file "day05")))
   (:module "day06"
    :components ((:file "day06")))))

