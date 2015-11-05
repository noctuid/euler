;;; Problem 1
(ql:quickload :alexandria)

(defun euler1 (bound)
  "Find the sum of the multiples of 3 or 5 from 1 to BOUND."
  (flet ((mult-of-3-or-5-p (num)
           (or (zerop (mod num 5))
               (zerop (mod num 3)))))
    (apply #'+
           (remove-if-not #'mult-of-3-or-5-p (alexandria:iota bound)))))

;;; Problem 2
;; lazy approach (thank you clazy)
(ql:quickload :clazy)

(defun lazy-fibonacci (&optional (n-2 0) (n-1 1))
  "Lazily creates a list corresponding to a fibonacci sequence.
N-2 and N-1 can be altered to change the starting values for the sequence."
  (lazy:call #'cons n-2 (lazy-fibonacci n-1 (+ n-1 n-2))))

(defun euler2 (max-value)
  "Sum even numbers in the fibonacci sequence less than MAX-VALUE.
Fibonacci sequence in this case starts with 1 2."
  (apply #'+ (remove-if #'oddp
                        (lazy-seqs:take-while
                         (lambda (x) (< x max-value))
                         (lazy-fibonacci 1 2)))))

;; (euler2 4000000) => 4613732
;; (euler2 9999999999999999999999999999999999999999999999999999999999) =>
;; 11885348277186225933407550847492422740019612693948321981990
