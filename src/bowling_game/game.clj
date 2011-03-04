(ns bowling-game.game
  (:use clojure.contrib.def))
  
(def sum (partial reduce +))

(defn- replace-last [seq item]
  (conj (vec (drop-last 1 seq)) item))

(defrecord Frame [rolls])
  
(defn- construct-frame [pins]
  (Frame. [pins]))

(defn- construct-empty-frame []
  (Frame. []))

(defrecord Game [frames])

(defn- construct-game []
  (Game. [(construct-empty-frame)]))
  
(defvar- current-game (atom (construct-game)))

(defn new-game []
  (reset! current-game (construct-game)))

(defvar- rolls-already-completed (comp count :rolls))	

(defvar- first-roll (comp first :rolls))	 

(defvar- second-roll (comp second :rolls)) 

(defvar- third-roll (comp last :rolls)) 

(defvar- pins-knocked-down (comp sum :rolls))

(defvar- all-pins-down? (comp (partial = 10) pins-knocked-down))
  
(defvar- all-rolls-done? (comp (partial = 2) rolls-already-completed))   

(defn- add-roll-to-frame [frame pins-hit-in-roll]
  (if-let [rolls (:rolls frame)]
    (assoc frame :rolls (conj rolls pins-hit-in-roll))
    (assoc frame :rolls (construct-frame pins-hit-in-roll))))	 
	
(defn- start-new-frame? [frame num-frames]
  (and (or (all-pins-down? frame) (all-rolls-done? frame))
       (> 10 num-frames)))	
	   
(defn- add-roll-to [frames pins-hit-in-roll]
  (let [current-frame (last frames)]
    (if (start-new-frame? current-frame (count frames))
      (conj frames (construct-frame pins-hit-in-roll))
      (replace-last
       frames
       (add-roll-to-frame current-frame pins-hit-in-roll)))))

(defn roll [pins-knocked-down]
  (reset! current-game
     (assoc
        @current-game
       :frames
       (add-roll-to (:frames @current-game) pins-knocked-down))))
	
(defn- all-pins-down-and-on-nth-roll? [nth-roll frame]
    (and (all-pins-down? frame)
	     (= (rolls-already-completed frame) nth-roll)))
	
(defvar- spare? (partial all-pins-down-and-on-nth-roll? 2))

(defvar- strike? (partial all-pins-down-and-on-nth-roll? 1))

(defn- next-roll-in-frames [[current-frame & _]]
  (first-roll current-frame))

(defn- next-two-rolls-in-frames [[current-frame next-frame & _ :as frames]]
  (let [last-frame? (nil? next-frame)]
    (cond last-frame?
         (+ (next-roll-in-frames frames) (second-roll current-frame))
         (strike? current-frame)
         (+ 10 (next-roll-in-frames frames))
		 :else
         (pins-knocked-down current-frame))))

(defn- score-current-frame [[current-frame & rest-frames]]
  (let [score (pins-knocked-down current-frame)]
     (cond (spare? current-frame)
        (+ score (next-roll-in-frames rest-frames))
        (strike? current-frame)
        (+ score (next-two-rolls-in-frames rest-frames))
        :else  
	    score)))
	
(defvar- next-roll-in-last-frame (comp second :rolls))

(defn- next-two-rolls-in-last-frame [frame]
  (+ (second-roll frame) (third-roll frame)))	
	  
(defn- score-last-frame [frame]
    (cond (spare? frame)
          (+ (first-roll frame) (next-roll-in-last-frame frame))
          (strike? frame)
          (+ (first-roll frame) (next-two-rolls-in-last-frame frame))
		  :else
          (pins-knocked-down frame)))

(defn- score-frames [score [current-frame & rest-frames :as frames-left]]
    (if (= 1 (count frames-left))
      (+ score (score-last-frame current-frame))
      (+ score (score-frames (score-current-frame frames-left) rest-frames))))

(defn score []
  (score-frames 0 (:frames @current-game)))