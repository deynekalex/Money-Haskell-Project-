module Handlers.AboutBtn where

import           Graphics.UI.Gtk      as Gtk hiding (get)

aboutText :: String
aboutText = "Я Александр Дейнека, студент 4 курса КТ. " ++ "\n" ++ "Это приложение несомненно облегчит вам учёт расходов."

aboutDeveloperBtnHandler :: IO ()
aboutDeveloperBtnHandler = do
    windowAbout <- windowNew
    mainBoxAbout <- vBoxNew False 1
    containerAdd windowAbout mainBoxAbout
    Gtk.set windowAbout [windowTitle := "О разработчике", windowDefaultWidth := 100,
                         windowDefaultHeight := 100, containerBorderWidth := 10, windowResizable := False]
    aboutEdt <- labelNew (Just aboutText)
    image <- imageNewFromFile "res/image.gif"
    boxPackStart mainBoxAbout aboutEdt PackGrow 5
    boxPackStart mainBoxAbout image PackGrow 0
    widgetShowAll windowAbout