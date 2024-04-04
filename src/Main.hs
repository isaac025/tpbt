{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Focus (FocusRing, focusNext, focusPrev, focusRing, focusSetCurrent, withFocusRing)
import Brick.Main (App (..), defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget, zoom)
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
import Brick.Widgets.List (List, list)
import Control.Monad (void)
import Data.Vector (empty)
import Graphics.Vty.Attributes (
    bold,
    brightBlue,
    brightWhite,
    defAttr,
    white,
    withStyle,
 )
import Graphics.Vty.Input (Event (EvKey), Key (..))
import Lens.Micro ((&), (^.))
import Lens.Micro.Mtl ((%=))
import Lens.Micro.TH (makeLenses)
import TPB

data Name = Search | ResultList | ContentList
    deriving (Eq, Ord, Show)

data State = State
    { _focus :: FocusRing Name
    , _results :: List Name Result
    , _contents :: List Name Content
    , _searchField :: Editor String Name
    }
makeLenses ''State

ui :: State -> [Widget Name]
ui s = [vCenter $ maxWidth 200 $ vBox [drawSearch, instructions]]
  where
    drawSearch = withBorderStyle unicodeBold $ border $ vLimit 1 $ label <+> e
    e = renderEditor (str . unlines) True (s ^. searchField)
    label = str "Search: "
    maxWidth w = hCenter . hLimit w
    instructions =
        maxWidth 100 $
            hBox
                [ drawInstr "Enter" "search/get contents"
                , drawInstr "D" "download"
                , drawInstr "/" "back to search"
                ]

drawList :: List Name Result -> Widget Name
drawList l = listUi
  where
    label = str "Results:"
    box = borderWithLabel
drawInstr :: String -> String -> Widget Name
drawInstr k a =
    withAttr attrKey (str k)
        <+> str " to "
        <+> withAttr attrBold (str a)
        & hCenter

initialState :: State
initialState =
    State
        { _focus = focusRing [Search, ResultList, ContentList]
        , _results = list ResultList empty 1
        , _contents = list ContentList empty 1
        , _searchField = editor Search Nothing ""
        }

event :: BrickEvent Name e -> EventM Name State ()
event (VtyEvent (EvKey KEsc [])) = halt
event (VtyEvent (EvKey (KChar '\t') [])) = focus %= focusNext
event (VtyEvent (EvKey (KChar '/') [])) = focus %= focusSetCurrent Search
event (VtyEvent (EvKey KBackTab [])) = focus %= focusPrev
event v = zoom searchField $ handleEditorEvent v

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

app :: App State e Name
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
