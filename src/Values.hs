{-# LANGUAGE TemplateHaskell    #-}
module Values where

import Utils
import Control.Monad.State
import Control.Monad
import Graphics.UI.Gtk hiding (get)
import Data.Acid
import Control.Monad.Reader
import Data.SafeCopy
import Data.Char
import Data.Typeable
import Data.List.Split
import Data.Time
import Control.Parallel.Strategies
import Control.Lens
import Types

mySplit :: [a] -> ([a], [a])
mySplit [] = ([], [])
mySplit [x] = ([x], [])
mySplit (x:y:xys) = (x:xs, y:ys) where (xs, ys) = mySplit xys

rewrap :: [Item] -> [(String,Double,Bool)]
rewrap (x:xs) = (x^.description, fromIntegral(x^.price), False) : (rewrap xs)
rewrap _ = []

getValues :: [Item] -> UTCTime -> UTCTime -> String -> [(String,Double,Bool)]
getValues list from to condition = runEval $ do
    let (a, b) = mySplit list
    a' <- rpar (rewrap $ (filter (\x -> (x^.typo == condition && x^.time < to && x^.time > from)) a))
    b' <- rpar (rewrap $ (filter (\x -> (x^.typo == condition && x^.time < to && x^.time > from)) b))
    return (a'++b')

--stack build && stack exec -- Money +RTS -N2 -s -l