(ns mu.core
   (:use clojure.pprint) 
   (require [clojure.string :as str]))

; Rules
;1.      xI      →       xIU     Add a U to the end of any string ending in I: MI      to      MIU
;2.      Mx      →       Mxx     Double the string after the M: MIU     to      MIUIU
;3.      xIIIy   →       xUy     Replace any III with a U:  MUIIIU  to      MUUU
;4.      xUUy    →       xy      Remove any UU: MUUU to MU
;Start with MI, get to MU

(def grow (atom true))

(defn rule1
  "xI -> xIU"
  [x]
  (if (= (last x) "I")
    (apply str (concat x "U"))
    x))

(defn rule2
  "Mx -> Mxx"
  [x]
  (let [y (rest x)]
    (apply str (concat "M" y y))))

(defn occurences
  [value target]
  (let [length (count target)]
    (loop [v (seq value) result '() index 0]
      (let [x (take length v)
            y (apply str x)]
        (if (empty? x)
          result
          (if (= y target)
            (recur (rest v) (conj result index) (+ index 1))
            (recur (rest v) result (+ index 1))))))))

(defn sub
  [x from to index]
  (let [to-str #(apply str %)
        front (to-str (take index x))
        back (to-str (drop (+ index (count from)) x))
        middle (to-str (drop index (take (+ index (count from)) x))) ]
    (if (= middle from)
      (str front to back)
      x)))

(defn rule3
  "mIII -> mU"
  [x index]
  (sub x "III" "U" index))

(defn rule4
  "mIUUII -> mIII"
  [x index]
  (sub x "UU" "" index))

(defn rule3-occurences [x] (occurences x "III"))
(defn rule4-occurences [x] (occurences x "UU"))

(defn require-index?
  [x]
  (= 2 (count (first (:arglists (meta x))))))

(defn select-rule
  []
  "Will select a rule based on if we are still growing the string. If true
  will select from rules 1 - 4. If false, will only select rules 3 and 4 as
  they shrink the size of the string."
  (let [number (if @grow 0 2)]
    (str "rule" (+ number (rand-int (- 4 number)) 1))))

(defn apply-random-rule
  [x]
  (let [rule-str (select-rule)
        rule (resolve (symbol rule-str))]
    (if (require-index? rule)
      (let [ox ((resolve (symbol (str rule-str "-occurences"))) x)]
        (if (empty? ox)
          x
          (rule x (nth ox (rand-int (count ox))))))
      (rule x))))

(defn run
  []
  (do
    (reset! grow true)
    (let [max-length 20
          iterations 50]
      (loop [value "MI" cnt iterations]
        (if (= cnt 0)
          value
          (do
            (if (> (count value) max-length) (reset! grow false))
            (recur (apply-random-rule value) (- cnt 1))))))))

(let [values (for [x (range 1 5000)
                   :let [y (run)]]
                   y)]
  (loop [v values results {}]
    (if (empty? v)
      (pprint
        (into
          (sorted-map-by
            (fn [a b] (< (count a) (count b))))
          results))
      (let [z (first v)]
        (recur (rest v)
               (assoc results
                      z
                      (+ 1 (if (nil? (get results z))
                             0
                             (get results z)))))))))
