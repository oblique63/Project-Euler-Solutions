; PROBLEM 2
; By considering the terms in the Fibonacci sequence whose values do not exceed four million,
; find the sum of the even-valued terms.

(defn fib [n]
  (if (<= n 3) n
      (+ (fib (- n 1))
         (fib (- n 2)) )))

(defn fibs [n]
  (map fib (range 1 (inc n))))

(defn fibs-to [max]
  (loop [n 1, highest 1]
    (if (>= highest max) (fibs (dec n))
        (recur (inc n) (last (fibs (inc n)))) )))

(defn sum-even-fibs [max]
  (reduce + (filter even? (fibs-to max))))