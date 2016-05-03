fact :: Float -> Float
fact 0 = 1
fact x = x * fact(x - 1)

expoList x = 1:[(x**i)/(fact i) | i<-[1..]]

ex :: Float -> Float
ex x = sum (take 50 (expoList x))
