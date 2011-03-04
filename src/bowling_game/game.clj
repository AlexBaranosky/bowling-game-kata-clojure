(ns bowling-game.game
  (:use clojure.contrib.def))
  
(def sum (partial reduce +))  

(defrecord Frame [pins-hit])
(defrecord Game [frames])

(defn- construct-frame [pins]
  (Frame. [pins]))

(defn- construct-empty-frame []
  (Frame. []))

(defn- construct-game []
  (Game. [(construct-empty-frame)]))

(defvar- current-game (atom (construct-game)))

(defn new-game []
  (reset! current-game (construct-game)))

(defvar- rolls-already-completed (comp count :pins-hit))	

(defvar- first-roll (comp first :pins-hit))	 

(defvar- second-roll (comp second :pins-hit)) 

(defvar- third-roll (comp last :pins-hit)) 

(defvar- pins-knocked-down (comp sum :pins-hit))

(defvar- all-pins-down? (comp (partial = 10) pins-knocked-down))
  
(defvar- all-rolls-done? (comp (partial = 2) rolls-already-completed))
	
(defn- start-new-frame? [frame num-frames]
  (and (or (all-pins-down? frame) (all-rolls-done? frame))
       (> 10 num-frames)))	   

(defn- add-pins-to-current-frame [frame pins]
  (if-let [pinlist (:pins-hit frame)]
    (assoc frame :pins-hit (conj pinlist pins))
    (assoc frame :pins-hit (construct-frame pins))))	 

(defn- replace-last [seq item]
  (conj (vec (drop-last 1 seq)) item))

(defn- add-pins-to-frames [frames pins]
  (let [current-frame (last frames)]
    (if (start-new-frame? current-frame (count frames))
      (conj frames (construct-frame pins))
      (replace-last
       frames
       (add-pins-to-current-frame current-frame pins)))))

(defn roll [pins]
  (reset! current-game
     (assoc
        @current-game
       :frames
       (add-pins-to-frames (:frames @current-game) pins))))
	
(defn- score-for-frame-is-10-and-on-roll-x? [rolls-already-rolled frame]
    (and (= rolls-already-rolled (rolls-already-completed frame)) 
	     (= 10 (pins-knocked-down frame))))
	
(defvar- spare? (partial score-for-frame-is-10-and-on-roll-x? 2))

(defvar- strike? (partial score-for-frame-is-10-and-on-roll-x? 1))

(defn- next-roll-in-frames [[first-frame & _]]
  (first-roll first-frame))

(defn- next-two-rolls-in-frames [[first-frame next-frame & _ :as frames]]
  (let [last-frame? (nil? next-frame)]
    (cond last-frame?
         (+ (next-roll-in-frames frames) (second-roll first-frame))
         (strike? first-frame)
         (+ 10 (next-roll-in-frames frames))
		 :else
         (pins-knocked-down first-frame))))

(defn- score-current-frame [[first-frame & rest-frames]]
  (let [score (pins-knocked-down first-frame)]
     (cond (spare? first-frame)
        (+ score (next-roll-in-frames rest-frames))
        (strike? first-frame)
        (+ score (next-two-rolls-in-frames rest-frames))
        :else  
	    score)))
	
(defvar- next-roll-in-last-frame (comp second :pins-hit))

(defn- next-two-rolls-in-last-frame [frame]
  (+ (second-roll frame) (third-roll frame)))	
	  
(defn- score-last-frame [frame]
    (cond (spare? frame)
          (+ (first-roll frame) (next-roll-in-last-frame frame))
          (strike? frame)
          (+ (first-roll frame) (next-two-rolls-in-last-frame frame))
		  :else
          (pins-knocked-down frame)))

(defn- score-frames [score [first-frame & rest-frames :as frames-left]]
    (if (= 1 (count frames-left))
      (+ score (score-last-frame first-frame))
      (+ score (score-frames (score-current-frame frames-left) rest-frames))))

(defn score []
  (score-frames 0 (:frames @current-game)))