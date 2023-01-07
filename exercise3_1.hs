sol = [(a,b,c,d,e,f,g,h,i,j) |
        a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], e <- [0..9], f <- [0..9], g <- [0..9], h <- [0..9], i <- [0..9], j <- [0..9],
      a * 100 + c*10 + e - a*100 + a*10 + a == a*10 + h,
    a * 100 + c*10 + e + d*100 + a *10 + c == j * 100+f*10+d,
       d*100 + a *10 + c + h*100 + f*10 + c == i*100 + i*10 + i,
        j*100+f*10+d - g*10 + i == j*100 + b*10 + j,
         a * 100 + a*10 + a - h*100 + f*10 + c == g*10 + i,
        a*10+ h + i*100+ i*10 + i == j*100 + b*10 + j, i /= j]




--aufgabe 4
--primes = 2: [x | x <- [3,5..],
--map (\t-> x `mod` t /= 0
--takeWhile (\y -> y^2 <= x) primes)]

--firstprimes :: n -> [int]


--let ace = a*100 + c*10 + e
           -- dac = d*100 + a*10 + c
            --aaa = a*100 + a*10 + a
            --ah = a*10 + h
            --hfc = h*100 + f*10 + c
            --iii = i*100 + i*10 + i
           -- jfd = j*100 + f*10 + d
           -- gi = g*10 +i
           -- jbj = j*100 + b*10 + j
           -- in ace + dac == jfd and in ace - aaa == ah and in dac + hfc == iii and in jfd - gi == jbj and in aaa+ hfc == gi and in ah + iii == jbj]


primes :: Integer -> [Integer]
primes n = 2: [x | x <- [3,5.. n],
 and (map (\t-> mod x t /= 0)
 (takeWhile (\y -> y^2 <= x) (primes x)))]


