(ns bowling-game.game)

(defrecord Frame [pins-hit-list])
(defrecord Game [frame-list])

(defn construct-frame [pins]
  (Frame. [pins]))

(defn construct-empty-frame []
  (Frame. []))

(defn construct-game []
  (Game. [(construct-empty-frame)]))

(def current-game (atom (construct-game)))

(defn new-game []
  (reset! current-game (construct-game)))

(defn all-pins-down-old? [frame]
  (let [frame-score (reduce + (:pins-hit-list frame))]
    (if (nil? frame-score)
      false
      (if (<= 10 frame-score)
        true
        false))))

(defn all-pins-down? [frame]
  (let [frame-score (reduce + (:pins-hit-list frame))]
    (if (<= 10 frame-score)
      true
      false)))

(defn all-rolls-done? [frame]
  (let [rolls-in-frame (count (:pins-hit-list frame))]
    (if (<= 2 rolls-in-frame)
      true
      false)))

(defn start-new-frame? [frame num-frames]
  (if (or (all-pins-down? frame) (all-rolls-done? frame))
    (if (> 10 num-frames)
      true
      false)
    false))

(defn add-pins-to-current-frame [frame, pins]
  (let [pinlist (:pins-hit-list frame)]
   (if (nil? pinlist)
     (assoc frame :pins-hit-list (construct-frame pins))
     (assoc frame :pins-hit-list (conj pinlist pins)))))

(defn replace-end-of-list [list, item]
  (vec (reverse (conj (rest (reverse list) ) item))))

(defn add-pins-to-frame-list [frame-list, pins]
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

(defn is-spare? [frame]
  (let [frame-score (reduce + (:pins-hit-list frame))
        try-count (count (:pins-hit-list frame))]
    (if (and (= 2 try-count) (= 10 frame-score))
      true
      false)))

(defn is-strike? [frame]
  (let [frame-score (reduce + (:pins-hit-list frame))
        try-count (count (:pins-hit-list frame))]
    (if (and (= 1 try-count) (= 10 frame-score))
      true
      false)))

(defn sum-frame [frame]
  (reduce + (:pins-hit-list frame)))

(defn get-next-roll-in-frame-list [frames]
  (first (:pins-hit-list (first frames))))

(defn get-next-two-rolls-in-frame-list [frames]
  (let [first-frame (first frames)
        next-frame (first (rest frames))
        last-frame? (nil? next-frame)]
    (if last-frame?
      (+ (first (:pins-hit-list first-frame)) (first (next (:pins-hit-list first-frame))))
      (if (is-strike? first-frame)
       (+ 10 (first (:pins-hit-list next-frame)))
       (sum-frame first-frame)))))


(defn get-next-roll-in-final-frame [frame]
  (first (next (:pins-hit-list frame))))

(defn get-next-two-rolls-in-final-frame [frame]
  (let [roll1 (first (next (:pins-hit-list frame)))
        roll2 (last (:pins-hit-list frame))]
    (+ roll1 roll2)))

(defn score-first-frame [frames]
  (let [first-frame (first frames)
        next-frames (rest frames)]
    (if (is-spare? first-frame)
      (+ (sum-frame first-frame) (get-next-roll-in-frame-list next-frames))
      (if (is-strike? first-frame)
        (+ (sum-frame first-frame) (get-next-two-rolls-in-frame-list next-frames))
        (sum-frame first-frame)
        ))))

(defn score-last-frame [frames]
  (let [last-frame (first frames)
        first-roll (first (:pins-hit-list (first frames)))]
    (if (is-spare? last-frame)
      (+ first-roll (get-next-roll-in-final-frame last-frame))
      (if (is-strike? last-frame)
        (+ first-roll (get-next-two-rolls-in-final-frame last-frame))
        (sum-frame last-frame)
        ))))

(defn sum-frames [score, frame-list]
  (let [num-frames (count frame-list)
        next-frames (rest frame-list)]
    (if (= 1 num-frames)
      (+ score (score-last-frame frame-list))
      (+ score (sum-frames (score-first-frame frame-list) next-frames)))))

(defn sum-frames-test [score, frame-list]
  (let [num-frames (count frame-list)
        next-frames (rest frame-list)
        score-first (score-first-frame frame-list)]
    (if (>= 2 num-frames)
      score
      (recur (+ score score-first) next-frames))))

(defn score []
  (sum-frames 0 (:frame-list @current-game)))



