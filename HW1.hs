module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where

-- do not modify the module declaration above!
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code 
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
-- Fellowship of the Grid (25, 5, 5, 5 points)
import Data.List
form :: [a] -> (Int, Int) -> [[a]] 
form [] _ = []
form list b = take(snd(b)) list : form (snd((splitAt (snd(b)) list))) b

constGrid :: a -> (Int, Int) -> [[a]]
constGrid a (0,_) = []
constGrid value tuple = take (snd(tuple)) (repeat value) : constGrid value ((fst(tuple)-1,snd(tuple)))


flatten :: [[a]] -> [a]
flatten formlist = concat formlist 

access :: [[a]] -> (Int, Int) -> a
access list tuple = head (snd(splitAt (snd(tuple)) (head (snd(splitAt (fst(tuple)) list)))))
----------------------------
-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice grid tuple1 tuple2 = map (fst) (map (splitAt (snd(tuple2)-fst(tuple2))) (map (snd) (map (splitAt (fst(tuple2))) (fst(splitAt (snd(tuple1)-fst(tuple1)) (snd(splitAt (fst(tuple1)) grid)))))))

vcat :: [[a]] -> [[a]] -> [[a]]
vcat [] grid2 = grid2
vcat (x:xs) grid2 = x : (vcat xs grid2)

hcat :: [[a]] -> [[a]] -> [[a]]
hcat [] grid2 = grid2
hcat grid1 grid2 = zipWith (++) grid1 grid2 

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without grid tuple1 tuple2 = zipWith (++) (map(fst)(map (splitAt (fst tuple2)) (fst(splitAt (fst(tuple1)) grid)++snd(splitAt(snd(tuple1))grid)))) (map(snd)(map (splitAt (snd tuple2)) (fst(splitAt (fst(tuple1)) grid)++snd(splitAt(snd(tuple1))grid))))

----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d grid pattern = ali grid pattern 0 0 []

ali grid pattern x y result= if ((y==(length(grid))))
                          then (result ++ [])
                       else if x == (length(grid!!0)-1)
                                  then 
                                    if (isit grid pattern x y)
                                          then  (ali grid pattern 0 (y+1)(result ++ [(y,x)]))
                                    else (ali grid pattern 0 (y+1) result)
                            else 
                              if (isit grid pattern x y)
                                          then (ali grid pattern (x+1) y (result ++ [(y,x)]) )
                                    else (ali grid pattern (x+1) y result)



sliceV2 grid pattern x y = (slice grid (y,(length(pattern)+y)) (x,(length(pattern!!0)+x)))

isit grid pattern x y = if ( sliceV2 grid pattern x y ) == pattern 
                          then True
                          else False

----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.