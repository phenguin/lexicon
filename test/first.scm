((lambda (a b) (+ (+ a a) (+ (if #t b (+ b 1)) 3))) 10 ((lambda (c) (+ 10 c)) 5))
