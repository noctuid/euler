\\ problem 2
\\ http://www.shenlanguage.org/learn-shen/functions/functions_higher_order.html
(define foldl
  F Z [] -> Z
  F Z [X | Xs] -> (foldl F (F Z X) Xs))

(define mod
  X Y -> (let Next-value (- X Y)
           (if (< Next-value Y)
               Next-value
               (mod Next-value Y))))

(define evenp
  X -> (= 0 (mod X 2)))

(define fibonacci-list
  N-2 N-1 Container Max-value ->
  (let N (+ N-1 N-2)
    (if (<= N Max-value)
        (fibonacci-list N-1 N (cons N (if (= Container [])
                                          [N-1 N-2]
                                          Container))
                        Max-value)
        Container)))

(define add-if-even
  Accum Next-val -> (if (evenp Next-val)
                        (+ Accum Next-val)
                        Accum))

(define euler2
  Max-value -> (foldl (function add-if-even)
                      0
                      (fibonacci-list 1 2 [] Max-value)))

\\ (euler2 4000000) => 4613732
