module Values where

import           Control.Lens                hiding (to,from)
import           Control.Parallel.Strategies
import           Data.Time
import           Types

--split it like [1,2,3,4,5,6] -> ([1,3,5],[2,4,6])
mySplit :: [a] -> ([a], [a])
mySplit [] = ([], [])
mySplit [x] = ([x], [])
mySplit (x:y:xys) = (x:xs, y:ys) where (xs, ys) = mySplit xys

--return relevant data to Diagram builder.
getValues :: [Item] -> UTCTime -> UTCTime -> String -> [Item]
getValues list from to condition = runEval $ do
    let (a, b) = mySplit list
    a' <- rpar (filter (\x -> x^.typo == condition && x^.time < to && x^.time > from) a)
    b' <- rpar (filter (\x -> x^.typo == condition && x^.time < to && x^.time > from) b)
    return (a'++b')

--stack build && stack exec -- Money +RTS -N2 -s -l
