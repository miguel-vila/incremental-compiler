(letrec ((e (lambda (x) (if (fxzero? x) #t (o (fxsub1 x)))))
         (o (lambda (x) (if (fxzero? x) #f (e (fxsub1 x))))))
       (e 5000000))
