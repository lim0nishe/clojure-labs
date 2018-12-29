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

(defn integral [f]
  (let [memF (memoize f)]
  (memoize (fn [x]
    (reduce +
      (for [index (range (* 10 x))]
        ((memoize trap) memF index)
        ))))
  ))