; PROBLEM 3
; Find the largest palindrome made from the product of two 3-digit numbers.

(defn combine [coll]
  "Returns pair combinations of the members of the original collection.
   ex: [1 2 3] -> [[1 1] [1 2] [1 3] [2 2] [2 3] [3 3]]
  "
  (loop [ i 0, v1 (coll i)
          j 0, v2 (coll j)
          out [] ]
    (cond
      (== i (count coll)) out
      (< j (count coll))
        (if (not (contains? (set out) [v2 v1]))
              (recur i v1 (inc j) (get coll (inc j)) (conj out [v1 v2]))
            (recur i v1 (inc j) (get coll (inc j)) out) )
      :else
      (recur (inc i) (get coll (inc i)) 0 (coll 0) out) )))

(defn mult-combos [coll]
  (map #(apply * %) (combine coll)))

(defn digit-vec [n]
  "Transforms an int into a vector of its digits.
   ex: 1234 -> [1 2 3 4]
  "
  (loop [i (count (str n)), digits []]
    (if (>= i 1)
        (recur (dec i)
               (conj digits
                     (int (/ (rem n (Math/pow 10 i))
                                    (Math/pow 10 (dec i)) ))))
      digits)))

(defn palindrome? [coll]
  (loop [l-index 0, r-index (dec (count coll))]
    (cond
      (> l-index r-index) true
      (= (coll l-index) (coll r-index))  (recur (inc l-index) (dec r-index))
      :else false )))


(defn palindromes [coll]
  (for [n (mult-combos (vec coll))
        :when (palindrome? (digit-vec n))] n))

(defn digit-range [n]
     (range (int (dec (Math/pow 10 n)))
            (int (dec (Math/pow 10 (dec n))))
            -1 ))

(defn largest-palindrome [int-range]
  (first (palindromes (digit-range int-range))))

