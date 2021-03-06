module Handlers.InfoDiagramBtn where

import           Control.Lens
import           Data.Time
import           Graphics.UI.Gtk                        as Gtk

import           Graphics.Rendering.Chart.Easy          hiding (label)
import           Graphics.Rendering.Chart.Gtk           as Chart

import           Types
import           Utils()
import           Values

--set size of pie and description
pitem :: Item -> PieItem
pitem item = pitem_value .~ fromIntegral(item^.price)
    $ pitem_label .~ item^.description
    $ pitem_offset .~ 0
    $ def

infoDiagramBtnHandler :: [Item] -> IO ()
infoDiagramBtnHandler quariedItemList = do
    windowDiagram <- windowNew
    Gtk.set windowDiagram [windowTitle := "Статистика", windowDefaultWidth := 500, windowDefaultHeight := 400, containerBorderWidth := 10, windowResizable := False]
    diagramEdt <- labelNew(Just "Отобразить статистику за период")

    --ForLeftBox
    leftEdt <- labelNew(Just "Начиная с")
    leftCal <- calendarNew
    calendarSetDisplayOptions leftCal [CalendarShowHeading,CalendarShowDayNames,CalendarWeekStartMonday]
    leftChosenDayLabel <- labelNew (Just "")
    miscSetAlignment leftChosenDayLabel 0.0 0.0

    -- Hour and minute choosing
    leftDayTimeBox <- hBoxNew False 0
    leftSpinH <- myAddSpinButton leftDayTimeBox "Часы:" 0.0 23.0 0.0
    leftSpinM <- myAddSpinButton leftDayTimeBox "Минуты:" 0.0 59.0 0.0
    leftConsBtn <- buttonNewWithLabel "Расходов"

    --ForRightBox
    rightEdt <- labelNew(Just "Заканчивая")
    rightCal <- calendarNew
    calendarSetDisplayOptions rightCal [CalendarShowHeading,CalendarShowDayNames,CalendarWeekStartMonday]
    rightChosenDayLabel <- labelNew (Just "")
    miscSetAlignment rightChosenDayLabel 0.0 0.0
    -- Hour and minute choosing
    rightDayTimeBox <- hBoxNew False 0
    rightSpinH <- myAddSpinButton rightDayTimeBox "Часы:" 0.0 23.0 23.0
    rightSpinM <- myAddSpinButton rightDayTimeBox "Минуты:" 0.0 59.0 59.0
    rightIncBtn <- buttonNewWithLabel "Доходов"

    --Boxing
    mainBoxDiagram <- vBoxNew False 1
    containerAdd windowDiagram mainBoxDiagram

    mainBoxDiagram1 <- hBoxNew False 1
    mainBoxDiagram2 <- hBoxNew False 1

    boxPackStart mainBoxDiagram mainBoxDiagram1 PackGrow 1
    boxPackStart mainBoxDiagram mainBoxDiagram2 PackGrow 1

    leftBox <- vBoxNew False 1
    boxPackStart leftBox leftEdt PackGrow 1
    boxPackStart leftBox leftCal PackGrow 1
    boxPackStart leftBox leftChosenDayLabel PackGrow 1
    boxPackStart leftBox leftDayTimeBox PackGrow 1
    boxPackEnd leftBox leftConsBtn PackGrow 10

    rightBox <- vBoxNew False 1
    boxPackStart rightBox rightEdt PackGrow 1
    boxPackStart rightBox rightCal PackGrow 1
    boxPackStart rightBox rightChosenDayLabel PackGrow 1
    boxPackStart rightBox rightDayTimeBox PackGrow 1
    boxPackEnd rightBox rightIncBtn PackGrow 10

    boxPackStart mainBoxDiagram1 diagramEdt PackGrow 1
    boxPackStart mainBoxDiagram2 leftBox PackGrow 1
    boxPackStart mainBoxDiagram2 rightBox PackGrow 1

    --leftBtn is show consumes
    _ <- ($) on leftConsBtn buttonActivated $ do
        --get date
        (year1, month1, day1) <- calendarGetDate leftCal
        --get hour and minute
        hour1 <- Gtk.get leftSpinH spinButtonValue
        minute1 <- Gtk.get leftSpinM spinButtonValue
        (year2, month2, day2) <- calendarGetDate rightCal
        hour2 <- Gtk.get rightSpinH spinButtonValue
        minute2 <- Gtk.get rightSpinM spinButtonValue
        Chart.toWindow 640 480 $ do
            --values is [Item] which suits to date range and typo
            let values = getValues quariedItemList
                            (UTCTime (fromGregorian (toInteger year1) (month1 + 1) day1)
                            (fromInteger (3600 * round hour1 + 60 * round minute1)))
                            (UTCTime (fromGregorian (toInteger year2) (month2 + 1) day2)
                            (fromInteger (3600 * round hour2 + 60 * round minute2)))
                            "Расход"
            let title = if null values then "Расходов в выбранном периоде нет" else "Расход"
            pie_title .= title
            pie_plot . pie_data .= map pitem values


    _ <- ($) on rightIncBtn buttonActivated $ do
        (year1', month1', day1') <- calendarGetDate leftCal
        hour1' <- Gtk.get leftSpinH spinButtonValue
        minute1 <- Gtk.get leftSpinM spinButtonValue
        (year2, month2, day2) <- calendarGetDate rightCal
        hour2' <- Gtk.get rightSpinH spinButtonValue
        minute2 <- Gtk.get rightSpinM spinButtonValue
        Chart.toWindow 640 480 $ do
            let values = getValues quariedItemList
                            (UTCTime (fromGregorian (toInteger year1') (month1' + 1) day1')
                            (fromInteger (3600 * round hour1' + 60 * round minute1)))
                            (UTCTime (fromGregorian (toInteger year2) (month2 + 1) day2)
                            (fromInteger (3600 * round hour2' + 60 * round minute2)))
                            "Доход"
            let title = if null values then "Доходов в выбранном периоде нет" else "Доходы"
            pie_title .= title
            pie_plot . pie_data .= map pitem values

    widgetShowAll windowDiagram

--creating spinHbox with some settings
myAddSpinButton :: HBox -> String -> Double -> Double -> Double -> IO SpinButton
myAddSpinButton box name minVal maxVal defaultValue = do
    vbox  <- vBoxNew False 0
    boxPackStart box vbox PackRepel 0
    label <- labelNew (Just name)
    miscSetAlignment label 0.0 0.5
    boxPackStart vbox label PackNatural 0
    spinb <- spinButtonNewWithRange minVal maxVal 1.0
    Gtk.set spinb [spinButtonValue := defaultValue]
    boxPackStart vbox spinb PackNatural 0
    return spinb