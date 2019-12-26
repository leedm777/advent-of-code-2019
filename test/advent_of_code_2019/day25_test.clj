(ns advent-of-code-2019.day25-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day25 :refer :all]
            [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]))

(deftest day25-part1
  (testing "parse-prompt"
    (is (= {:name  "Hull Breach"
            :descr "You got in through a hole in the floor here. To keep your ship from also freezing, the hole has been sealed."
            :doors ["north" "east" "west"]
            :items []
            :neighbors {}}
           (parse-prompt (s/join "\n" ["== Hull Breach =="
                                       "You got in through a hole in the floor here. To keep your ship from also freezing, the hole has been sealed."
                                       ""
                                       "Doors here lead:"
                                       "- north"
                                       "- east"
                                       "- west"
                                       ""
                                       "Command?"]))))
    (is (= {:name  "Corridor"
            :descr "The metal walls and the metal floor are slightly different colors. Or are they?"
            :doors ["north" "east" "south"]
            :items ["infinite loop"]
            :neighbors {}}
           (parse-prompt (s/join "\n" ["== Corridor =="
                                       "The metal walls and the metal floor are slightly different colors. Or are they?"
                                       ""
                                       "Doors here lead:"
                                       "- north"
                                       "- east"
                                       "- south"
                                       ""
                                       "Items here:"
                                       "- infinite loop"
                                       ""
                                       "Command?"]))))))
