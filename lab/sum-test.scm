(letrec ((sum (lambda (n ac)
                (if (fxzero? n)
                    ac
                    (sum (fxsub1 n) (fx+ n ac))))))
  (sum 64 0))
