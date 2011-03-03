(ns bowling-game.test.game
  (:use [bowling-game.game] :reload)
  (:use [clojure.test])
  (:use [midje.sweet]))

(defn roll-many [n pins]
  (dorun (map roll (repeat n pins))))

(defn roll-spare []
  (do
    (roll 5)
    (roll 5)))

(defn roll-strike []
  (do
    (roll 10)))

(deftest replaceLastElementWithGivenElement
  (fact (replace-last [1 2 3] 44) => [1 2 44]))
	
(deftest testGutterGame
  (do
    (new-game)
    (roll-many 20 0)
    (fact (score) => 0)))

(deftest testAllOnes
  (do
    (new-game)
    (roll-many 20 1)
    (fact (score) => 20)))

(deftest testOneSpare
  (do
    (new-game)
    (roll-spare)
    (roll 3)
    (roll-many 16 0)
    (fact (score) => 16)))

(deftest testOneStrike
  (do
    (new-game)
    (roll-strike)
    (roll 3)
    (roll 4)
    (roll-many 16 0)
    (fact (score) => 24)))

(deftest testPerfectGame
  (do
    (new-game)
    (roll-many 12 10)
    (fact (score) => 300)))

(deftest testAllFivesGame
  (do
    (new-game)
    (roll-many 21 5)
    (fact (score) => 150)))
