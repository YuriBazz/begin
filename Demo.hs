module Demo where 

factorial n = 
    if n < 0 
        then error "АШИБКА"
        else 
            if n == 0
                then 1
                else n * factorial (n - 1)

fibn n = 
    if n <= 2
        then 1 
        else fibn (n - 1) + fibn (n - 2)

factorial' 0 = 1
factorial' n = n * factorial' (n - 1) 

factorial'' 0 = 1 
factorial'' n = if n < 0 then undefined else n * factorial'' (n - 1)

doubleFact n = 
    if n <= 0  
        then 1
        else n * doubleFact (n - 2) 

undefinedTest n = if n > 1 then 1 else undefined 

factorial''' n 
   | n < 0  = undefined
   | n > 0  = n * factorial''' (n - 1)
   | n == 0 = 1

slowFibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | n < 0 = slowFibonacci (n + 2) - slowFibonacci (n + 1)
    | n > 0 = slowFibonacci (n - 1) + slowFibonacci (n - 2)

-- U can write iterational "factorial" but I don't want to do that

iterf cur next num
     | num == 0 = cur
     | num > 0 = iterf next (next + cur) (num - 1)
     | num < 0 = iterf next (cur - next) (num + 1)
fibonacci = iterf 0 1  -- Useless step but I like it

roots :: Double -> Double -> Double -> (Double, Double)
roots a b c = 
    let   
        d = sqrt (b ^ 2 - 4 * a * c) 
        x1 = (-b + d) / (2 * a)
        x2 = (-b - d) / (2 * a)
    in (x1, x2)

theBestFibonacci n =
    let iter1 cur next num  
    | num == 0 = cur
    | num > 0 = iter1 next (next + cur) (num - 1)
    | num < 0 = iter1 next (cur - next) (num + 1)
    in iter1 0 1 n

{- I have to realise: a0 = 1 
                      a1 = 2
                      a2 = 3
                      ak = a(k-1) + a(k-2) - 2 * a(k-3) -}

seqA n = 
    let iter n3 n2 n1 n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = n3
    | n > 2 = iter (n3 + n2 - 2 * n1) n3 n2 (n - 1)
        in iter 3 2 1 n
 
