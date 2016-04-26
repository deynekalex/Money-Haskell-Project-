{-# LANGUAGE TemplateHaskell    #-}
module Utils where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad
import Graphics.UI.Gtk hiding (get)
import Data.Acid
import Control.Monad.Reader
import Data.SafeCopy
import Data.Char
import Data.Typeable
import Data.List.Split
import Data.Time
import Control.Lens
import Types

instance Ord Item where
    task1 <= task2 = task1^.time <= task2^.time

instance Show Item where
    show i = i^.typo ++ " - " ++
                (show (i^.price)) ++ " - " ++
                i^.description ++ "(" ++
                (formatTime defaultTimeLocale format (i^.time)) ++ ")" where
                    format = "%d/%m/%Y (%a) %H:%M"

isValidPrice :: String  -> Bool
isValidPrice curPrice = not (null curPrice) && all isNumber curPrice

getValidPrice :: Entry -> MaybeT IO Int
getValidPrice addPriceEdt = do
    curPrice <- lift (entryGetText addPriceEdt)
    guard (isValidPrice curPrice)
    return (read curPrice)

getBalance :: [Item] -> Int
getBalance list = foldr (+) 0 (map func list)

func :: Item -> Int
func item = if item^.typo == "Доход" then item^.price else (-item^.price)

--interact with db
insert :: Item -> Update ItemList ()
insert item = do
     ItemList ns <- get
     put (ItemList (ns ++ [item]))

deleteByPos :: Int -> Update ItemList ()
deleteByPos pos = do
    ItemList ns <- get
    put (ItemList (take (pos - 1) ns ++ drop (pos + 1) ns))

edit :: Int -> Item -> Update ItemList ()
edit pos item = do
    ItemList ns <- get
    put (ItemList (take (pos - 1) ns ++ [item] ++ drop (pos + 1) ns))

getItemList :: Query ItemList [Item]
getItemList = do
    ItemList ns <- ask
    return ns