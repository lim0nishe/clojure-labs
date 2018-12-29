(defn trap [f index]
  (*
    (/
      (+
        (f (* index 0.1))
        (f (* (+ index 1) 0.1))
        )
      2
      )
    (-
      (* (+ index 1) 0.1)
      (* index 0.1)
      )
    ))

(defn integralSeq
  ([f] (integralSeq f 1 0))
  ([f, n, collector]
   (lazy-seq (cons
               collector
               (integralSeq f (inc n) (+ collector (trap f n)))))))

(defn lazyIntegral [f]
  (fn [x]
    (nth (integralSeq f) (* 10 x))))