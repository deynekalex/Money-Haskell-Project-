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

getValues :: [Item] -> String -> [(String,Double,Bool)]
getValues list condition = rewrap $ (filter (\x -> typo x == condition) list)
--TODO добавить monad Ether

myAddSpinButton :: HBox -> String -> Double -> Double -> IO SpinButton
myAddSpinButton box name min max = do
    vbox  <- vBoxNew False 0
    boxPackStart box vbox PackRepel 0
    label <- labelNew (Just name)
    miscSetAlignment label 0.0 0.5
    boxPackStart vbox label PackNatural 0
    spinb <- spinButtonNewWithRange min max 1.0
    boxPackStart vbox spinb PackNatural 0
    return spinb

data Time = Time { year   :: Int
                    , month :: Int
                    , day    :: Int
                    , hour   :: Int
                    , minute :: Int
                } deriving (Typeable, Eq, Ord)