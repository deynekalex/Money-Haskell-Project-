module Handlers.TreeSelection where

import           Control.Lens         hiding (index)
import           Control.Monad
import           Graphics.UI.Gtk      as Gtk hiding (get)
import           Types

onSelectionChangedHandler :: ListStore Item -> Entry -> Entry -> TreeSelection -> IO ()
onSelectionChangedHandler itemList editPriceEdt editDescEdt selection = do
    selRows <- treeSelectionGetSelectedRows selection
    unless (null selRows) $ do
        let index = head (head selRows)
        v <- listStoreGetValue itemList index
        entrySetText editPriceEdt (show (v^.price))
        entrySetText editDescEdt (v^.description)
