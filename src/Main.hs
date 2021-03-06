{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies       #-}
module Main where
import           Control.Lens              hiding (index)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Acid
import           Data.List                 hiding (insert)
import           Data.SafeCopy
import           Data.Time
import           Graphics.UI.Gtk           as Gtk hiding (get)
import           Types

--local imports
import           Handlers.AboutBtn
import           Handlers.InfoDiagramBtn
import           Handlers.TreeSelection
import           System.IO.Unsafe
import           Utils

--AcidState
$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''ItemList)
$(makeAcidic ''ItemList ['insert, 'deleteByPos, 'edit, 'getItemList])

main :: IO()
main = do
    --Read saved items and sort it
    database <- openLocalState (ItemList [])
    quariedItemList <- query database GetItemList
    itemList <- listStoreNew []
    mapM_ (listStoreAppend itemList) (sort quariedItemList)

    --Initialize GUI
    _ <- initGUI
    window <- windowNew
    Gtk.set window [windowTitle := "Учёт расходов", windowDefaultWidth := 500,
                    windowDefaultHeight := 400, containerBorderWidth := 10]
    --mainBox
    mainBox <- vBoxNew False 1
    containerAdd window mainBox

    --aboutDeveloper
    aboutDeveloperBtn <- buttonNewWithLabel "О разработчике"

    --descriptionLabel
    histlbl <- labelNew(Just "История расходов/доходов")

    --ListView
    treeview <- treeViewNewWithModel itemList
    treeViewSetHeadersVisible treeview False
    col <- treeViewColumnNew
    rend <- cellRendererTextNew
    cellLayoutPackStart col rend False
    cellLayoutSetAttributes col rend itemList
        (\i -> [cellText := show i])
    _ <- treeViewAppendColumn treeview col
    -- Make ability to scroll task's list
    itemsScrwin <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport itemsScrwin treeview
    scrolledWindowSetPolicy itemsScrwin PolicyAutomatic PolicyAutomatic
    --set selection
    selection <- treeViewGetSelection treeview
    treeViewColumnsAutosize treeview
    treeSelectionSetMode selection SelectionSingle

    --infoBox
    infoBox <-  hBoxNew False 1
    infoBox1 <- hBoxNew False 1
    infoBox2 <- hBoxNew False 1
    let curBalance = getBalance quariedItemList
    balanceLbl <- labelNew(Just $ show curBalance)
    infoLbl <- labelNew(Just ("Баланс = " ++ show curBalance))
    infoDiagramBtn <- buttonNewWithLabel "Статистика"
    boxPackStart infoBox1 infoLbl PackGrow 1
    boxPackEnd infoBox2 infoDiagramBtn PackGrow 1
    boxPackStart infoBox infoBox1 PackGrow 148
    boxPackEnd infoBox infoBox2 PackGrow 1

    --DescriptionPrice
    descPriceBox <- hBoxNew False 1
    descPriceBox1 <- vBoxNew False 1
    descPriceBox2 <- vBoxNew False 1
    descPriceBox3 <- vBoxNew False 1
    descPriceBox4 <- vBoxNew False 1
    descLbl <- labelNew(Just "Описание")
    priceLbl <- labelNew(Just "Цена")
    boxPackStart descPriceBox2 descLbl PackGrow 0
    boxPackStart descPriceBox3 priceLbl PackGrow 0
    boxPackStart descPriceBox descPriceBox1 PackGrow 1
    boxPackStart descPriceBox descPriceBox2 PackGrow 20
    boxPackStart descPriceBox descPriceBox3 PackGrow 20
    boxPackStart descPriceBox descPriceBox4 PackGrow 35

    --addBox
    addBox <- hBoxNew False 2
    addBox1 <- vBoxNew False 2
    addBox2 <- vBoxNew False 2
    addBox3 <- vBoxNew False 2
    addBox4 <- vBoxNew False 2
    addBox41 <- hBoxNew False 2
    addLbl <- labelNew(Just "Новый:")
    addDescEdt <- entryNew
    addPriceEdt <- entryNew
    addConsBtn <- buttonNewWithLabel "Расход"
    addIncBtn <- buttonNewWithLabel "Доход"
    boxPackStart addBox1 addLbl PackGrow 0
    boxPackStart addBox2 addDescEdt PackGrow 0
    boxPackStart addBox3 addPriceEdt PackGrow 0
    boxPackStart addBox41 addConsBtn PackGrow 0
    boxPackStart addBox41 addIncBtn PackGrow 0
    boxPackStart addBox4 addBox41 PackGrow 0
    boxPackStart addBox addBox1 PackGrow 0
    boxPackStart addBox addBox2 PackGrow 0
    boxPackStart addBox addBox3 PackGrow 0
    boxPackStart addBox addBox4 PackGrow 0

    --editBox
    editBox <- hBoxNew False 2
    editBox1 <- vBoxNew False 2
    editBox2 <- vBoxNew False 2
    editBox3 <- vBoxNew False 2
    editBox4 <- vBoxNew False 2
    edtLbl <- labelNew(Just "Изменить:")
    editDescEdt <- entryNew
    editPriceEdt <- entryNew
    saveBtn <- buttonNewWithLabel "Сохранить"
    boxPackStart addBox1 edtLbl PackGrow 0
    boxPackStart addBox2 editDescEdt PackGrow 0
    boxPackStart addBox3 editPriceEdt PackGrow 0
    boxPackStart addBox4 saveBtn PackGrow 0
    boxPackStart editBox editBox1 PackGrow 0
    boxPackStart editBox editBox2 PackGrow 0
    boxPackStart editBox editBox3 PackGrow 0
    boxPackStart editBox editBox4 PackGrow 0

    --delButton
    delBtn <- buttonNewWithLabel "Удалить"

    --packing into mainBox
    boxPackStart mainBox aboutDeveloperBtn PackNatural 0
    boxPackStart mainBox histlbl PackNatural 0
    boxPackStart mainBox itemsScrwin PackGrow 0
    boxPackStart mainBox infoBox PackNatural 0
    boxPackStart mainBox descPriceBox PackNatural 0
    boxPackStart mainBox addBox PackNatural 0
    boxPackStart mainBox editBox PackNatural 0
    boxPackStart mainBox delBtn PackNatural 0

    --adding new consume
    _ <- ($) onClicked addConsBtn $ do
        --check for not null and all isDigit
        curPrice <- runMaybeT $ getValidPrice addPriceEdt
        case curPrice of
            Nothing -> do
                --no comment
                dialog <- messageDialogNew (Just window) [DialogDestroyWithParent]
                    MessageError ButtonsNone "Цена должна состоять только из цифр"
                widgetShowAll dialog
            Just curPrice' -> do
                --get description of consume
                curDescription <- entryGetText addDescEdt
                now <- getCurrentTime
                unless (null curDescription) $ do
                    let newItem1 = Item "Расход" curDescription curPrice' now
                    --add to treeSelection
                    _ <- listStoreAppend itemList newItem1
                    --add to database
                    update database (Insert newItem1)
                    --update current balance of user
                    curBalance' <- labelGetText balanceLbl
                    let updatedBalance1 = show (read curBalance' - curPrice')
                    labelSetText infoLbl ("Баланс = " ++ updatedBalance1)
                    labelSetText balanceLbl updatedBalance1
                    entrySetText addDescEdt ""
                    entrySetText addPriceEdt ""

    --all is the same as above, except income
    --adding new income
    _ <- ($) onClicked addIncBtn $  do
        curPrice <- runMaybeT $ getValidPrice addPriceEdt
        case curPrice of
            Nothing -> do
                dialog <- messageDialogNew (Just window) [DialogDestroyWithParent]
                    MessageError ButtonsNone "Цена должна состоять только из цифр"
                widgetShowAll dialog
            Just curPrice' -> do
                curDescription <- entryGetText addDescEdt
                now <- getCurrentTime
                unless (null curDescription) $ do
                    let newItem2 = Item "Доход" curDescription curPrice' now
                    _ <- listStoreAppend itemList newItem2
                    update database (Insert newItem2)
                    curBalance' <- labelGetText balanceLbl
                    let updatedBalance2 = show (read curBalance' + curPrice')
                    labelSetText infoLbl ("Баланс = " ++ updatedBalance2)
                    labelSetText balanceLbl updatedBalance2
                    entrySetText addDescEdt ""
                    entrySetText addPriceEdt ""

    --edit consume/income
    _ <- ($) onClicked saveBtn $ do
        selRows <- treeSelectionGetSelectedRows selection
        unless (length selRows < 1) $ do
            let index = head (head selRows)
            curPrice <- runMaybeT $ getValidPrice editPriceEdt
            case curPrice of
                Nothing -> do
                    dialog <- messageDialogNew (Just window) [DialogDestroyWithParent]
                        MessageError ButtonsNone "Цена должна состоять только из цифр"
                    widgetShowAll dialog
                Just curPrice' -> do
                    curDescription <- entryGetText editDescEdt :: IO String
                    v <- listStoreGetValue itemList index
                    unless (null curDescription) $ do
                        let newItem = Item (v^.typo) curDescription curPrice' (v^.time)
                        --edit treeView
                        listStoreSetValue itemList index newItem
                        --edit database
                        update database (Edit index newItem)
                        --update current balance of user
                        let curBalance' = read $ unsafePerformIO $ labelGetText balanceLbl
                        let updatedBalance = show $ if newItem^.typo == "Доход" then curBalance' + curPrice' - (v^.price) else curBalance' - curPrice' + (v^.price)
                        labelSetText infoLbl ("Баланс = " ++ updatedBalance)
                        labelSetText balanceLbl updatedBalance
                        entrySetText editDescEdt ""
                        entrySetText editPriceEdt ""

    --delete income/consume
    _ <- ($) onClicked delBtn $ do
        --get selected rows
        selRows <- treeSelectionGetSelectedRows selection
        unless (null selRows) $ do
            let index = head (head selRows)
            --edit treeView
            v <- listStoreGetValue itemList index
            let curBalance' = read $ unsafePerformIO $ labelGetText balanceLbl
            --update current balance of user
            let updatedBalance = show $ if v^.typo == "Доход" then curBalance' - (v^.price) else curBalance' + (v^.price)
            labelSetText infoLbl ("Баланс = " ++ updatedBalance)
            labelSetText balanceLbl updatedBalance
            --remove from database
            update database (DeleteByPos index)
            listStoreRemove itemList index
            entrySetText editDescEdt ""
            entrySetText editPriceEdt ""

    --set entry's values to selectied item values
    _ <- ($) onSelectionChanged selection $ onSelectionChangedHandler itemList editPriceEdt editDescEdt selection

    --open new window with some info about developer
    _ <- ($) onClicked aboutDeveloperBtn aboutDeveloperBtnHandler

    --open new window, which configurates diagrams
    _ <- ($) onClicked infoDiagramBtn $ do
        updQuariedItemList <- query database GetItemList
        infoDiagramBtnHandler updQuariedItemList

    _ <- onDestroy window mainQuit
    _ <- onDestroy window (closeAcidState database)
    widgetShowAll window
    mainGUI
