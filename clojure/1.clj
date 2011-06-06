; PROBLEM 1: 
; Find the sum of all the multiples of 3 or 5 below 1000.

(defn multiple?
  "Returns whether or not 'n' is a multiple of any of the given factors"
  ([n factor] (= 0 (rem n factor)))
  ([n factor & more]
    (cond (nil? factor) false
          (multiple? n factor) true
          :else (recur n (first more) (rest more)) )))


(defn multiples-upto
  "Returns all the multiples of the given factors, upto the 'max'"
  ([max factor]
     (filter #(multiple? % factor)
             (range max) ))
  ([max factor & more]
     (filter #(apply multiple? (flatten [% factor more]))
             (range max) )))


(defn sum-multiples-upto
  "Returns the sum of all the multiples of the given factors, upto the 'max'"
  ([max factor]
    (reduce + (multiples-upto max factor)))
  ([max factor & more]
    (reduce + (apply multiples-upto (flatten [max factor more]) ))))
