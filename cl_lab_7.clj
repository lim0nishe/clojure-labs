(defn constant [bool]
  {:pre [(boolean? bool)]}
  (list ::const bool)
  )

(defn constant? [expr]
  (= (first expr) ::const)
  )

(defn constant-value [c]
  (second c)
  )

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name)
  )

(defn variable? [expr]
  (= (first expr) ::var)
  )

(defn variable-name [v]
  (second v)
  )

(defn same-variables? [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1)
       (variable-name v2)))
  )

(defn args [expr]
  (rest expr)
  )

(defn invert [expr]
  (list ::inv expr)
  )

(defn invert? [expr]
  (= (first expr) ::inv)
  )

(defn subexpr [expr]
  {:pre [(invert? expr)]}
  (second expr)
  )

(defn conjunct [expr & rest]
  (cons ::conj (cons expr rest))
  )

(defn conjunct? [expr]
  (= (first expr) ::conj)
  )

(defn disjunct [expr & rest]
  (cons ::disj (cons expr rest))
  )

(defn disjunct? [expr]
  (= (first expr) ::disj)
  )

(defn implicat [exprl exprr]
  (cons ::impl (list exprl exprr))
  )

(defn implicat? [expr]
  (= (first expr) ::impl)
  )

(defn left [expr]
  {:pre [(implicat? expr)]}
  (second expr)
  )

(defn right [expr]
  {:pre [(implicat? expr)]}
  (nth expr 2)
  )

;; правила вывода
(declare dnf)
(def bool-rules
  (list
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]
    [(fn [expr] (invert? expr))
     (fn [expr]
       (cond
         (constant? (subexpr expr))
         (constant (not (constant-value (subexpr expr))))
         (variable? (subexpr expr))
         expr
         (invert? (subexpr expr))
         (dnf (subexpr (subexpr expr)))
         (disjunct? (subexpr expr))
         (dnf (apply conjunct (map #(dnf (invert %)) (args (subexpr expr))) ) )
         (conjunct? (subexpr expr))
         (dnf (apply disjunct (map #(dnf (invert %)) (args (subexpr expr))) ) )
         (implicat? (subexpr expr))
         (dnf (conjunct (left (subexpr expr)) (invert (right (subexpr expr)))))
         )
       )]
    [(fn [expr] (implicat? expr))
     (fn [expr]
       (dnf (disjunct
                    (invert (left expr))
                    (right expr)))
       )]
    [(fn [expr] (disjunct? expr))
     (fn [expr]
       (apply disjunct (mapcat #(if (disjunct? %)
                                  (map (fn [arg] (dnf arg)) (args %) )
                                  (list (dnf %))
                                  )
                               (args expr))) )]
    [(fn [expr] (conjunct? expr))
     (fn [expr]
       (let [flattened (mapcat #(if (conjunct? %)
                                  (map (fn [arg] (dnf arg)) (args %) )
                                  (list (dnf %))
                                  )
                               (args expr) )
             found_disjunction (first (filter (fn [arg] (disjunct? arg)) flattened) )
             ]
         (if (nil? found_disjunction)
           flattened
           (apply disjunct
                  (map
                    (fn [arg]
                      (apply conjunct (cons arg (filter
                                                  (fn [conjunction] (not (identical? conjunction found_disjunction)) )
                                                  flattened
                                                  )))
                      )
                    (args found_disjunction))
                  )
           )
         )
       )]
    )
  )

(defn dnf [expr]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         bool-rules)
    expr))



