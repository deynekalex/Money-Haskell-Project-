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

rewrap :: [Item] -> [(String,Double,Bool)]
rewrap (x:xs) = (description x, fromIntegral $ price $ x, False) : (rewrap xs)
rewrap _ = []

getValues :: [Item] -> UTCTime -> UTCTime -> String -> [(String,Double,Bool)]
getValues list from to condition = rewrap $ (filter (\x -> (typo x == condition && time x < to && time x > from)) list)
--TODO добавить monad Ether