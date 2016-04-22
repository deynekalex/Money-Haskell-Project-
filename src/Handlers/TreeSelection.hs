module Handlers.TreeSelection where

import Control.Monad.State
import Control.Monad
import Graphics.UI.Gtk as Gtk hiding (get)
import Data.Acid
import Control.Monad.Reader
import Data.SafeCopy
import Data.Char
import Data.List.Split
import Data.List hiding (insert)
import Data.Time

import Utils

onSelectionChangedHandler itemList editPriceEdt editDescEdt selection = do
    selRows <- treeSelectionGetSelectedRows selection
    unless (null selRows) $ do
        let index = head (head selRows)
        v <- listStoreGetValue itemList index
        entrySetText editPriceEdt (show (price  v))
        entrySetText editDescEdt (description v)