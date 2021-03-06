Module to generate randomness.

> module RNG (
>   RandomNumberGenerator,
>   rngWithSeed,           -- :: Int -> RandomNumberGenerator
>   generateNumber,        -- :: Int -> Int -> RandomNumberGenerator -> (Int,RandomNumberGenerator)
>   splitRng,              -- :: RandomNumberGenerator -> (RandomNumberGenerator,RandomNumberGenerator)
>   shuffleList            -- :: [a] -> RandomNumberGenerator -> ([a], RandomNumberGenerator)
> )
> where

This type will hold our current seed for the next number.

> type RandomNumberGenerator = Int

> rngWithSeed :: Int -> RandomNumberGenerator
> rngWithSeed seed = seed

> nextRng :: RandomNumberGenerator -> RandomNumberGenerator
> nextRng rng = (((a*rng `mod` m)*rng `mod` m)+(b*rng `mod` m)+c) `mod` m
>     where a = 18211
>           b = 32183
>           c = 23993
>           m = 50503

This function generates a random number in an interval [from,to] and updates the random number generator.

> generateNumber :: Int -> Int -> RandomNumberGenerator -> (Int,RandomNumberGenerator)
> generateNumber from to rng = (newrng `mod` (to-from+1) + from, newrng)
>     where newrng = nextRng rng

This function takes an RNG and splits into two RNGs.

> splitRng :: RandomNumberGenerator -> (RandomNumberGenerator,RandomNumberGenerator)
> splitRng rng = (nextRng (rng+1),nextRng (rng+2))

> shuffleList :: [a] -> RandomNumberGenerator -> ([a], RandomNumberGenerator)
> shuffleList []     rng = ([],rng)
> shuffleList (x:xs) rng = (insertAt pos x shuffledList,rng3)
>     where (shuffledList,rng2) = shuffleList xs rng
>           (pos,rng3)          = generateNumber 0 (length xs) rng2

> insertAt :: Int -> a -> [a] -> [a]
> insertAt 0 x ys     = x:ys
> insertAt n x (y:ys) = y : insertAt (n-1) x ys
