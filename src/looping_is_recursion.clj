(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc e]
                 (if (zero? e)
                   acc
                   (recur (* acc base) (dec e))))]
    (helper 1 exp)))


(defn last-element [a-seq]
  (let [helper (fn [a-seq acc]
                 (if (empty? a-seq)
                   acc
                   (if (empty? (rest a-seq))
                     (recur (rest a-seq) (first a-seq))
                     (recur (rest a-seq) nil))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2 acc]
                 (cond
                  (and (empty? s1) (empty? s2)) acc
                  (or (empty? s1) (empty? s2)) (recur '() '() false)
                  (= (first s1) (first s2)) (recur (rest s1) (rest s2) true)
                  :else (recur '() '() false)))]
    (helper seq1 seq2 true)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         a a-seq
         acc2 nil]
    (if (empty? a)
      acc2
      (if (pred (first a))
        acc
        (recur (inc acc) (rest a) acc2)))))

(defn avg [a-seq]
  (loop [acc 0 ab a-seq count 0]
    (if (empty? ab)
      (/ acc count)
      (recur (+ acc (first ab)) (rest ab) (inc count)))))

(defn parity [a-seq]
  (loop [acc #{} a a-seq]
    (if (empty? a)
      acc
      (let [f (first a)]
        (if (contains? acc f)
          (recur (disj acc f) (rest a))
          (recur (conj acc f) (rest a)))))))


(defn fast-fibo [n]
  (loop [a 1 b 1 acc 0]
    (cond
     (= a 0) acc
     (<= a 2) (recur 0 0 (inc acc))
     :else (recur (dec a) (dec b) (+ acc a b)))))

(defn cut-at-repetition [a-seq]
  [":("])

