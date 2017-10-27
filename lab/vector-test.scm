(let ([v (make-vector 5 2)])
  (do (vector-set! v 1 42)
      (vector-ref v 1)))
