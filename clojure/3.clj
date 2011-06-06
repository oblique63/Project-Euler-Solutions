; PROBLEM 3
; What is the largest prime factor of the number 600851475143 ?

(defn factor? [x n]
  (== 0 (rem n x)))

(defn factors [n]
  (filter #(factor? % n)
          (range 1 (inc n)) ))

(defn prime? [n]
  (== 2 (count (take 3 (factors n)))))

(defn prime-factor? [pf n]
  (and (factor? pf n) (prime? pf)))

(defn prime-factors [n]
  (filter #(prime-factor? % n)
          (range 1 (inc n)) ))

(defn largest-prime-factor [n]
  (loop [i n]
    (if (prime-factor? i n) i
        (recur (dec i)) )))

; Alternate versions:
(defn largest-prime-factor2 [n]
  (first (filter #(prime-factor? % n)
                 (range (inc n) 1 -1) )))

(defn largest-prime-factor3 [n]
  (last (prime-factors n)))
