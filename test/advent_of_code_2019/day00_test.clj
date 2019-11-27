(ns advent-of-code-2019.day00-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day00 :refer :all]))

(deftest day00-test
  (testing "example 1"
    (is (= 3 (:total (day00 "+1
-2
+3
+1
")))))

  (testing "example 2"
    (is (= 3 (:total (day00 "+1
+1
+1
")))))

  (testing "example 3"
    (is (= 0 (:total (day00 "+1
+1
-2
")))))

  (testing "example 4"
    (is (= -6 (:total (day00 "-1
-2
-3
")))))

  (testing "part two example 1"
    (is (= 0 (:first-dupe (day00 "+1
-1
")))))
  (testing "part two example 2"
    (is (= 10 (:first-dupe (day00 "+3
+3
+4
-2
-4
")))))

  (testing "part two example 3"
    (is (= 5 (:first-dupe (day00 "-6
+3
+8
+5
-6
")))))

  (testing "part two example 4"
    (is (= 14 (:first-dupe (day00 "+7
+7
-2
-7
-4
"))))))
