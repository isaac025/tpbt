{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusRing, focusSetCurrent, withFocusRing)
import Brick.Main (App (..), defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget, zoom)
import Brick.Util (fg)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core (
    hBox,
    hLimit,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
 )
import Brick.Widgets.Edit (
    Editor,
    editFocusedAttr,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
 )
import Brick.Widgets.List (List, handleListEvent, list, listName, renderList)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
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
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import TPB

data Name = Search | ResultList | ContentList
    deriving (Eq, Ord)

instance Show Name where
    show Search = "Search"
    show ResultList = "Results"
    show ContentList = "Result Contents"

data State = State
    { _focus :: FocusRing Name
    , _resultsList :: List Name Result
    , _contentsList :: List Name Content
    , _searchField :: Editor String Name
    }
makeLenses ''State

ui :: State -> [Widget Name]
ui s = [vCenter $ maxWidth 200 $ vBox content]
  where
    content = [searchUi, listsUi, instrsUi]
    searchUi = withBorderStyle unicodeBold $ border $ vLimit 1 $ label <+> e
    e = renderEditor (str . unlines) True (s ^. searchField)
    label = str "Search: "
    maxWidth w = hCenter . hLimit w
    toListUi r lns =
        let isActive = focusGetCurrent (s ^. focus) == r
         in s ^. lns & drawList isActive
    listsUi =
        hBox
            [ hCenter $ toListUi (Just ResultList) resultsList
            , str " "
            , hCenter $ toListUi (Just ContentList) contentsList
            ]
    instrsUi =
        maxWidth 100 $
            hBox
                [ drawInstr "Enter" "search/get contents"
                , drawInstr "D" "download"
                , drawInstr "/" "back to search"
                ]

drawList :: (Show e) => Bool -> List Name e -> Widget Name
drawList hasF l = listUi
  where
    listUi =
        withBorderStyle unicode $
            borderWithLabel (drawTitle l) $
                renderList drawEls hasF l
    drawTitle = str . show . listName

drawEls :: (Show e) => Bool -> e -> Widget Name
drawEls _ = str . show

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
        , _resultsList = list ResultList empty 1
        , _contentsList = list ContentList empty 1
        , _searchField = editor Search Nothing ""
        }

event :: BrickEvent Name e -> EventM Name State ()
event (VtyEvent (EvKey KEsc [])) = halt
event (VtyEvent (EvKey (KChar '\t') [])) = focus %= focusNext
event (VtyEvent (EvKey (KChar '/') [])) = focus %= focusSetCurrent Search
event (VtyEvent (EvKey KBackTab [])) = focus %= focusPrev
event e = do
    f <- focusGetCurrent <$> use focus
    case f of
        (Just Search) -> handleSearch e
        (Just ResultList) -> undefined -- zoom resultsList $ handleListEvent e
        (Just ContentList) -> undefined -- zoom contentsList $ handleListEvent e
        Nothing -> pure ()
event _ = pure ()

handleSearch :: BrickEvent Name e -> EventM Name State ()
handleSearch (VtyEvent (EvKey KEnter [])) = do
    c <- unwords . getEditContents <$> use searchField
    let fs = mkSearchFields c
    (Results res) <- liftIO $ runTpb fs (pirateSearch >> currentRes)
    resultsList .= list ResultList res 1
  where
    mkSearchFields s = SearchFields s Audio
handleSearch e = zoom searchField $ handleEditorEvent e

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
