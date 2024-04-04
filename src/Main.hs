module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Main (App (..), defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget)
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core (hBox, hLimit, str, vBox, vLimit, withAttr, withBorderStyle, (<+>))
import Brick.Widgets.Edit (
    Editor,
    editFocusedAttr,
    editor,
    handleEditorEvent,
    renderEditor,
 )
import Control.Monad (void)
import Graphics.Vty.Attributes (
    bold,
    brightBlue,
    brightWhite,
    defAttr,
    white,
    withStyle,
 )
import Graphics.Vty.Input (Event (EvKey), Key (..))
import Lens.Micro ((&))

data Name = Search | ResultList | ContentList
    deriving (Eq, Ord, Show)

ui :: Editor String () -> [Widget ()]
ui s = [vCenter $ maxWidth 200 $ vBox [drawSearch, instructions]]
  where
    drawSearch = withBorderStyle unicodeBold $ border $ vLimit 1 $ label <+> e
    e = renderEditor (str . unlines) True s
    label = str "Search: "
    maxWidth w = hCenter . hLimit w
    instructions =
        maxWidth 100 $
            hBox
                [ drawInstr "Enter" "search/get contents"
                , drawInstr "D" "download"
                , drawInstr "/" "back to search"
                ]

drawInstr :: String -> String -> Widget ()
drawInstr k a =
    withAttr attrKey (str k)
        <+> str " to "
        <+> withAttr attrBold (str a)
        & hCenter

initialState :: Editor String ()
initialState = editor () Nothing ""

event :: BrickEvent () e -> EventM () (Editor String ()) ()
event (VtyEvent (EvKey (KChar 'q') [])) = halt
event (VtyEvent (EvKey KEsc [])) = halt
event v = handleEditorEvent v

attrBold :: AttrName
attrBold = attrName "bold"

attrKey :: AttrName
attrKey = attrName "key"
theme :: AttrMap
theme =
    attrMap
        defAttr
        [ (editFocusedAttr, fg brightWhite)
        , (attrBold, withStyle (fg white) bold)
        , (attrKey, withStyle (fg brightBlue) bold)
        ]

app :: App (Editor String ()) e ()
app =
    App
        { appDraw = ui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = event
        , appStartEvent = pure ()
        , appAttrMap = const theme
        }

main :: IO ()
main = void $ defaultMain app initialState
