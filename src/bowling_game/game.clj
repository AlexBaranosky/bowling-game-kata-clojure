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

(defvar- rolls-in-frame (comp count :pins-hit))	

(defvar- first-roll (comp first :pins-hit))	  

(defvar- sum-frame (comp sum :pins-hit))

(defvar- all-pins-down? (comp (partial = 10) sum-frame))
  
(defvar- all-rolls-done? (comp (partial = 2) rolls-in-frame))
	
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
    (and (= rolls-already-rolled (rolls-in-frame frame)) 
	     (= 10 (sum-frame frame))))
	
(defvar- spare? (partial score-for-frame-is-10-and-on-roll-x? 2))

(defvar- strike? (partial score-for-frame-is-10-and-on-roll-x? 1))

(defn- next-roll-in-frames [[first-frame & _]]
  (first (:pins-hit first-frame)))

(defn- next-two-rolls-in-frames [[first-frame next-frame & _ :as frames]]
  (let [last-frame? (nil? next-frame)]
    (cond last-frame?
         (+ (next-roll-in-frames frames) (second (:pins-hit first-frame)))
         (strike? first-frame)
         (+ 10 (next-roll-in-frames frames))
		 :else
         (sum-frame first-frame))))

(defvar- next-roll-in-final-frame (comp second :pins-hit))

(defn- next-two-rolls-in-final-frame [frame]
  (let [roll1 (second (:pins-hit frame))
        roll2 (last (:pins-hit frame))]
    (+ roll1 roll2)))

(defn- score-current-frame [[first-frame & rest-frames]]
  (cond (spare? first-frame)
        (+ (sum-frame first-frame) (next-roll-in-frames rest-frames))
      (strike? first-frame)
        (+ (sum-frame first-frame) (next-two-rolls-in-frames rest-frames))
      :else  
	  (sum-frame first-frame)))
	  
(defn- score-last-frame [frame]
    (cond (spare? frame)
          (+ (first-roll frame) (next-roll-in-final-frame frame))
          (strike? frame)
          (+ (first-roll frame) (next-two-rolls-in-final-frame frame))
		  :else
          (sum-frame frame)))

(defn- sum-frames [score [first-frame & rest-frames :as frames-left]]
    (if (= 1 (count frames-left))
      (+ score (score-last-frame first-frame))
      (+ score (sum-frames (score-current-frame frames-left) rest-frames))))

(defn score []
  (sum-frames 0 (:frames @current-game)))