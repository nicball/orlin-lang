(define global-var 5)

(define empty-array
  (fn [key int] 0))

(define get-array
  (fn [arr (-> int int)
       key int]
    (arr key)))

(define const
  (fn [i int]
    (fn [_ unit] i)))

(define test
  (fn [i0 int, i1 int, i01 int]
    (let [i 1]
      (let [i 2]
        i))))

(define main
  (fn [u unit]
    (get-array empty-array 0)
    (let [n (const 666)]
      (test 2 33 3)
      (n unit))))

