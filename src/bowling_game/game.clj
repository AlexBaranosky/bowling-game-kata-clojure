(ns bowling-game.game)
  
(def sum (partial reduce +))  

(defrecord Frame [pins-hit-list])
(defrecord Game [frame-list])

(defn- construct-frame [pins]
  (Frame. [pins]))

(defn- construct-empty-frame []
  (Frame. []))

(defn- construct-game []
  (Game. [(construct-empty-frame)]))

(def current-game (atom (construct-game)))

(defn new-game []
  (reset! current-game (construct-game)))

(defn- all-pins-down? [frame]
  (let [frame-score (sum (:pins-hit-list frame))]
    (<= 10 frame-score)))

(defn rolls-in-frame [frame]
   (count (:pins-hit-list frame)))	
	
(defn- all-rolls-done? [frame]
    (<= 2 (rolls-in-frame frame)))
	
(defn- start-new-frame? [frame num-frames]
  (and (or (all-pins-down? frame) (all-rolls-done? frame))
       (> 10 num-frames)))	   
	   
(defn- add-pins-to-current-frame [frame pins]
  (let [pinlist (:pins-hit-list frame)]
   (if (nil? pinlist)
     (assoc frame :pins-hit-list (construct-frame pins))
     (assoc frame :pins-hit-list (conj pinlist pins)))))

(defn- replace-end-of-list [list item]
  (vec (reverse (conj (rest (reverse list) ) item))))

(defn- add-pins-to-frame-list [frame-list pins]
  (let [current-frame (last frame-list)
        new-frame (construct-frame pins)
        num-frames (count frame-list)]
    (if (start-new-frame? current-frame num-frames)
      (conj frame-list new-frame)
      (replace-end-of-list
       frame-list
       (add-pins-to-current-frame current-frame pins)))))

(defn roll [pins]
  (reset! current-game
     (assoc
        @current-game
       :frame-list
       (add-pins-to-frame-list (:frame-list @current-game) pins))))

(defn- sum-frame [frame]
  (sum (:pins-hit-list frame)))
	   
; major duplication between this and strike?
(defn- spare? [frame]
    (and (= 2 (rolls-in-frame frame)) 
	     (= 10 (sum-frame frame))))

(defn- strike? [frame]
    (and (= 1 (rolls-in-frame frame)) 
	     (= 10 (sum-frame frame))))

(defn- next-roll-in-frame-list [[first-frame & _]]
  (first (:pins-hit-list first-frame)))

(defn- next-two-rolls-in-frame-list [[first-frame next-frame & _]]
  (let [last-frame? (nil? next-frame)]
    (if last-frame?
      (+ (first (:pins-hit-list first-frame)) (second (:pins-hit-list first-frame)))
      (if (strike? first-frame)
       (+ 10 (first (:pins-hit-list next-frame)))
       (sum-frame first-frame)))))

(defn- next-roll-in-final-frame [frame]
  (second (:pins-hit-list frame)))

(defn- next-two-rolls-in-final-frame [frame]
  (let [roll1 (second (:pins-hit-list frame))
        roll2 (last (:pins-hit-list frame))]
    (+ roll1 roll2)))

(defn- score-first-frame [[first-frame & rest-frames]]
  (if (spare? first-frame)
      (+ (sum-frame first-frame) (next-roll-in-frame-list rest-frames))
      (if (strike? first-frame)
        (+ (sum-frame first-frame) (next-two-rolls-in-frame-list rest-frames))
        (sum-frame first-frame))))

(defn- score-last-frame [[last-frame & nada]]
  (let [first-roll (first (:pins-hit-list last-frame))]
    (if (spare? last-frame)
      (+ first-roll (next-roll-in-final-frame last-frame))
      (if (strike? last-frame)
        (+ first-roll (next-two-rolls-in-final-frame last-frame))
        (sum-frame last-frame)))))

(defn- sum-frames [score [_ & next-frames :as frame-list]]
  (let [num-frames-left (count frame-list)]
    (if (= 1 num-frames-left)
      (+ score (score-last-frame frame-list))
      (+ score (sum-frames (score-first-frame frame-list) next-frames)))))

(defn score []
  (sum-frames 0 (:frame-list @current-game)))