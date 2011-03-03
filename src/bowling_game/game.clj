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

(defn- rolls-in-frame [frame]
   (count (:pins-hit frame)))	

(defn- sum-frame [frame]
  (sum (:pins-hit frame)))

(defn- all-pins-down? [frame]
    (= 10 (sum-frame frame)))
  
(defn- all-rolls-done? [frame]
    (= 2 (rolls-in-frame frame)))
	
(defn- start-new-frame? [frame num-frames]
  (and (or (all-pins-down? frame) (all-rolls-done? frame))
       (> 10 num-frames)))	   
	   
(defn- add-pins-to-current-frame [frame pins]
  (let [pinlist (:pins-hit frame)]
   (if (nil? pinlist)
     (assoc frame :pins-hit (construct-frame pins))
     (assoc frame :pins-hit (conj pinlist pins)))))

(defn- add-pins-to-current-frame [frame pins]
  (if-let [pinlist (:pins-hit frame)]
    (assoc frame :pins-hit (conj pinlist pins))
    (assoc frame :pins-hit (construct-frame pins))))	 
	 
(defn- replace-end-of-list [list item]
  (vec (reverse (conj (rest (reverse list)) item))))

(defn- add-pins-to-frames [frames pins]
  (let [current-frame (last frames)
        new-frame (construct-frame pins)
        num-frames (count frames)]
    (if (start-new-frame? current-frame num-frames)
      (conj frames new-frame)
      (replace-end-of-list
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

(defn- next-two-rolls-in-frames [[first-frame next-frame & _]]
  (let [last-frame? (nil? next-frame)]
    (cond last-frame?
         (+ (first (:pins-hit first-frame)) (second (:pins-hit first-frame)))
         (strike? first-frame)
         (+ 10 (first (:pins-hit next-frame)))
		 :else
         (sum-frame first-frame))))

(defn- next-roll-in-final-frame [frame]
  (second (:pins-hit frame)))

(defn- next-two-rolls-in-final-frame [frame]
  (let [roll1 (second (:pins-hit frame))
        roll2 (last (:pins-hit frame))]
    (+ roll1 roll2)))

(defn- score-first-frame [[first-frame & rest-frames]]
  (cond (spare? first-frame)
        (+ (sum-frame first-frame) (next-roll-in-frames rest-frames))
      (strike? first-frame)
        (+ (sum-frame first-frame) (next-two-rolls-in-frames rest-frames))
      :else  
	  (sum-frame first-frame)))

(defn- score-last-frame [last-frame]
  (let [first-roll (first (:pins-hit last-frame))]
    (cond (spare? last-frame)
          (+ first-roll (next-roll-in-final-frame last-frame))
          (strike? last-frame)
          (+ first-roll (next-two-rolls-in-final-frame last-frame))
		  :else
          (sum-frame last-frame))))

(defn- sum-frames [score [first-frame & rest-frames :as frames]]
  (let [num-frames-left (count frames)]
    (if (= 1 num-frames-left)
      (+ score (score-last-frame first-frame))
      (+ score (sum-frames (score-first-frame frames) rest-frames)))))

(defn score []
  (sum-frames 0 (:frames @current-game)))