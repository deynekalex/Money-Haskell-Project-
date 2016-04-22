{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Main where
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

--for diagrams
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk as Chart

--local imports
import Utils
import Values
import Handlers.AboutBtn
import Handlers.TreeSelection
import Handlers.InfoDiagramBtn

$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''ItemList)
$(makeAcidic ''ItemList ['insert, 'deleteByPos, 'edit, 'getItemList])

{-
values :: [ (String,Double,Bool) ]
values = [ ("Mexico City",19.2,False), ("Mumbai",12.9,False), ("Sydney",4.3,False), ("London",8.3,False), ("New York",8.2,True) ]
-}

main :: IO()
main = do
    --Read saved notes
    database <- openLocalState (ItemList [])
    quariedItemList <- query database GetItemList
    itemList <- listStoreNew []
    mapM_ (listStoreAppend itemList) (sort quariedItemList)

{-    Chart.toWindow 640 480 $ do
        pie_title .= "Relative Population"
        pie_plot . pie_data .= map pitem (getValues (quariedItemList) ("Расход"))-}

    --Initialize GUI
    initGUI
    window <- windowNew
    Gtk.set window [windowTitle := "Учёт расходов", windowDefaultWidth := 500, windowDefaultHeight := 400, containerBorderWidth := 10]
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
    let format = "(%a) %d/%m/%Y %H:%M"
    cellLayoutSetAttributes col rend itemList
        (\i -> [cellText := typo i ++ " - " ++ (show (price i)) ++ " - " ++ description i ++ "(" ++ (formatTime defaultTimeLocale format (time i)) ++ ")"])
    treeViewAppendColumn treeview col
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
    let curBalance = getBalance (ItemList quariedItemList)
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

    onClicked addConsBtn $ do
        curDescription <- entryGetText addDescEdt :: IO String
        curPrice <- entryGetText addPriceEdt :: IO String
        now <- getCurrentTime
        when (not (null curDescription) && not (null curPrice) && all isNumber curPrice) $ do
            let newItem = Item "Расход" curDescription (read curPrice) now
            listStoreAppend itemList newItem
            update database (Insert newItem)
            curBalance <- labelGetText balanceLbl
            let updatedBalance = show ((read curBalance) - (read curPrice))
            labelSetText infoLbl ("Баланс = " ++ updatedBalance)
            labelSetText balanceLbl updatedBalance
            entrySetText addDescEdt ""
            entrySetText addPriceEdt ""

    onClicked addIncBtn $ do
        curDescription <- entryGetText addDescEdt :: IO String
        curPrice <- entryGetText addPriceEdt :: IO String
        now <- getCurrentTime
        when (not (null curDescription) && not (null curPrice) && all isNumber curPrice) $ do
            let newItem = Item "Доход" curDescription (read curPrice) now
            listStoreAppend itemList newItem
            update database (Insert newItem)
            curBalance <- labelGetText balanceLbl
            let updatedBalance = show ((read curBalance) + (read curPrice))
            labelSetText infoLbl ("Баланс = " ++ updatedBalance)
            labelSetText balanceLbl updatedBalance
            entrySetText addDescEdt ""
            entrySetText addPriceEdt ""

    onClicked saveBtn $ do
        selRows <- treeSelectionGetSelectedRows selection
        unless (length selRows < 1) $ do
            let index = head (head selRows)
            curDescription <- entryGetText editDescEdt :: IO String
            curPrice <- entryGetText editPriceEdt :: IO String
            v <- listStoreGetValue itemList index
            when (not (null curDescription) && not (null curPrice) && all isNumber curPrice) $ do
                let newItem = Item (typo v) curDescription (read curPrice) (time v)
                listStoreSetValue itemList index newItem
                update database (Edit index newItem)
                curBalance <- labelGetText balanceLbl
                let updatedBalance = if typo newItem == "Доход" then show ((read curBalance) + (read curPrice) - (price v)) else show ((read curBalance) - (read curPrice) + (price v))
                labelSetText infoLbl ("Баланс = " ++ updatedBalance)
                labelSetText balanceLbl updatedBalance
                entrySetText editDescEdt ""
                entrySetText editPriceEdt ""

    onClicked delBtn $ do
        selRows <- treeSelectionGetSelectedRows selection
        unless (null selRows) $ do
            let index = head (head selRows)
            v <- listStoreGetValue itemList index
            curBalance <- labelGetText balanceLbl
            let updatedBalance = if typo v == "Доход" then show ((read curBalance) - (price v)) else show ((read curBalance) + (price v))
            labelSetText infoLbl ("Баланс = " ++ updatedBalance)
            labelSetText balanceLbl updatedBalance
            update database (DeleteByPos index)
            listStoreRemove itemList index
            entrySetText editDescEdt ""
            entrySetText editPriceEdt ""

    onSelectionChanged selection $ do onSelectionChangedHandler itemList editPriceEdt editDescEdt selection

    onClicked aboutDeveloperBtn $ do aboutDeveloperBtnHandler

    onClicked infoDiagramBtn $ do
        quariedItemList <- query database GetItemList
        infoDiagramBtnHandler quariedItemList

    onDestroy window mainQuit
    onDestroy window (closeAcidState database)
    widgetShowAll window
    mainGUI