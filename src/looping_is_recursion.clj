(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [x y n]
                 (cond
                  (= 1 n)
                    x
                  (zero? n)
                    1
                  :else
                    (recur (* x y) y (dec n))))]
    (helper base base exp)))

(defn last-element [a-seq]
  (let [helper (fn [sequ]
                 (cond
                  (empty? sequ)
                    nil
                  (= 1 (count sequ))
                    (first sequ)
                  :else
                    (recur (rest sequ))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                  (and (empty? a-seq) (empty? b-seq))
                    true
                  (or (and (nil? (first a-seq)) (nil? (first b-seq)))
                      (not (= (first a-seq) (first b-seq))))
                    false
                  :else
                    (recur (rest a-seq) (rest b-seq))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [pred? pred
         sequ  a-seq
         n     0]
    (cond
     (empty? sequ)
       nil
     (pred? (first sequ))
       n
     :else
       (recur pred? (rest sequ) (inc n)))))

(defn avg [a-seq]
  (loop [sequ a-seq
         sum  0
         n    1]
    (if (empty? sequ)
      (/ sum (dec n))
      (recur (rest sequ) (+ sum (first sequ)) (inc n)))))

(defn parity [a-seq]
  (loop [seq1 a-seq
         set1 #{}]
    (let [toggle (fn [set2 elem]
                   (if (contains? set2 elem)
                     (disj set2 elem)
                     (conj set2 elem)))]
      (if (empty? seq1)
        set1
        (recur (rest seq1)
               (toggle set1 (first seq1)))))))

(defn fast-fibo [n]
  (loop [fib-1 1
         fib   1
         x     (- n 2)]
    (cond
     (zero? n)
       0
     (or (= 1 n) (= 2 n))
       1
     (zero? x)
       fib
     :else
       (recur fib (+ fib-1 fib) (dec x)))))

(defn cut-at-repetition [a-seq]
  (loop [from-seq    a-seq
         to-seq      []
         to-seq-has  #{}]
    (if (or (empty? from-seq) (contains? to-seq-has (first from-seq)))
      to-seq
      (recur (rest from-seq)
             (conj to-seq (first from-seq))
             (conj to-seq-has (first from-seq))))))

