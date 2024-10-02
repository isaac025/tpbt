{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Focus (
    FocusRing,
    focusGetCurrent,
    focusNext,
    focusPrev,
    focusRing,
    focusRingCursor,
    focusSetCurrent,
 )
import Brick.Main (App (..), defaultMain, halt)
import Brick.Types (BrickEvent (..), CursorLocation, EventM, Widget)
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core (
    Padding (..),
    emptyWidget,
    hBox,
    hLimitPercent,
    padAll,
    padRight,
    str,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
 )
import Brick.Widgets.Dialog (
    Dialog,
    buttonAttr,
    buttonSelectedAttr,
    dialog,
    dialogAttr,
    dialogSelection,
    handleDialogEvent,
    renderDialog,
 )
import Brick.Widgets.Edit (
    Editor,
    editFocusedAttr,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
 )
import Brick.Widgets.List (
    List,
    handleListEvent,
    list,
    listClear,
    listSelectedAttr,
    listSelectedElement,
    listSelectedFocusedAttr,
    renderList,
 )
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Vector (Vector, empty, fromList)
import Graphics.Vty.Attributes (
    black,
    bold,
    brightBlue,
    brightWhite,
    brightYellow,
    defAttr,
    withStyle,
 )
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (MCtrl))
import Lens.Micro ((&), (^.), _Just)
import Lens.Micro.Mtl (use, zoom, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import TPB.Monad
import TPB.Types
import Prelude hiding (null)

data Name
    = SearchBar
    | CategorySelection
    | ResultList
    | SongList
    | DownloadOpt
    | BackOpt
    deriving (Ord, Eq, Show)

data Choice = Back | Download Result
    deriving (Show)

data TpbSt = TpbSt
    { _results :: Vector Result
    , _searchBar :: Editor String Name
    , _resultList :: List Name Result
    , _dialogOpt :: Maybe (Dialog Choice Name)
    , _categorySel :: List Name Audio
    , _fRing :: FocusRing Name
    }
makeLenses ''TpbSt

initState :: TpbSt
initState =
    TpbSt
        { _results = empty
        , _searchBar = editor SearchBar (Just 1) ""
        , _resultList = list ResultList empty 1
        , _dialogOpt = Nothing
        , _categorySel = list CategorySelection (fromList [Music .. Other]) 1
        , _fRing = focusRing [SearchBar, CategorySelection, ResultList, DownloadOpt, BackOpt]
        }

ui :: TpbSt -> [Widget Name]
ui TpbSt{..} = drawDialog : [vCenter $ padAll 1 $ searchBarUi <=> l1 <+> str " " <+> l2 <=> instructions]
  where
    l1 = drawList (Just ResultList == focusGetCurrent _fRing) _resultList
    l2 = hLimitPercent 20 $ drawList (Just CategorySelection == focusGetCurrent _fRing) _categorySel

    drawDialog =
        case _dialogOpt of
            Nothing -> emptyWidget
            Just d -> renderDialog d (str "Download?")
    drawList isF ls =
        withBorderStyle unicode $
            borderWithLabel (str "Results") $
                renderList drawListElement isF ls

    drawListElement isFocused ltype =
        if isFocused
            then padRight Max $ str $ " " <> show ltype
            else str $ " " <> show ltype

    searchBarUi :: Widget Name
    searchBarUi = withBorderStyle unicode $ border $ vLimit 1 $ label <+> searchBarEditor
      where
        label = str "Search: "
        searchBarEditor = renderEditor (str . unlines) True _searchBar

    instructions :: Widget Name
    instructions =
        hBox
            [ drawInstruction "Enter" "Search"
            , drawInstruction "Tab" "move to next"
            , drawInstruction "/" "go to search bar"
            , drawInstruction "C-q" "quit"
            , drawInstruction "C-k" "quit"
            , drawInstruction "Esc" "quit"
            ]

    drawInstruction :: String -> String -> Widget Name
    drawInstruction key cmd =
        withAttr attrKey (str key)
            <+> str " to "
            <+> withAttr attrBold (str cmd)
            & hCenter

syncResultList :: Vector Result -> EventM Name TpbSt ()
syncResultList v = do
    results .= v
    resultList %= listClear
    resultList .= list ResultList v 1

appEvents :: BrickEvent Name e -> EventM Name TpbSt ()
appEvents bv@(VtyEvent ev)
    | isQuit ev = halt
    | otherwise = do
        d <- use dialogOpt
        if isJust d
            then appHandleEventDialog ev
            else appHandleEventMain bv
  where
    isQuit :: Event -> Bool
    isQuit (EvKey (KChar 'k') [MCtrl]) = True
    isQuit (EvKey (KChar 'q') [MCtrl]) = True
    isQuit (EvKey KEsc []) = True
    isQuit _ = False

    download :: Result -> EventM Name TpbSt ()
    download t = do
        liftIO $ downloadTorrent t
        fRing %= focusSetCurrent ResultList

    back :: EventM Name TpbSt ()
    back = fRing %= focusSetCurrent ResultList

    appHandleEventDialog :: Event -> EventM Name TpbSt ()
    appHandleEventDialog (EvKey KEnter []) = do
        d <- use dialogOpt
        case dialogSelection =<< d of
            Just (DownloadOpt, Download t) -> download t
            Just (BackOpt, Back) -> back
            _ -> pure ()
    appHandleEventDialog (EvKey KEsc []) = back
    appHandleEventDialog (EvKey (KChar 'b') []) = back
    appHandleEventDialog (EvKey (KChar 'q') []) = back
    appHandleEventDialog e = zoom (dialogOpt . _Just) $ handleDialogEvent e
appEvents _ = pure ()

appHandleEventMain :: BrickEvent Name e -> EventM Name TpbSt ()
appHandleEventMain (VtyEvent (EvKey (KChar '/') [])) = fRing %= focusSetCurrent SearchBar
appHandleEventMain (VtyEvent (EvKey (KChar '\t') [])) = fRing %= focusNext
appHandleEventMain (VtyEvent (EvKey KBackTab [])) = fRing %= focusPrev
appHandleEventMain (VtyEvent (EvKey KEnter [])) = do
    f <- focusGetCurrent <$> use fRing
    case f of
        Just SearchBar -> searchTpb
        Just CategorySelection -> searchTpb
        Just ResultList -> do
            r <- listSelectedElement <$> use resultList
            case r of
                Nothing -> pure ()
                Just (_, res) -> do
                    fRing %= focusSetCurrent DownloadOpt
                    dialogOpt .= Just (dialog (Just (str "Title")) (Just (DownloadOpt, choices res)) 50)
        _ -> pure ()
  where
    choices r =
        [ ("Download", DownloadOpt, Download r)
        , ("Back", BackOpt, Back)
        ]
    searchTpb :: EventM Name TpbSt ()
    searchTpb = do
        s <- unlines . getEditContents <$> use searchBar
        c <- listSelectedElement <$> use categorySel
        case c of
            Nothing -> pure ()
            Just (_, cat) -> do
                req <- liftIO $ mkRequest (Left (s, toCat cat))
                res <- liftIO $ fetch @Result req
                syncResultList res
appHandleEventMain ev@(VtyEvent e) = do
    r <- use fRing
    case focusGetCurrent r of
        Just SearchBar -> zoom searchBar $ handleEditorEvent ev
        Just ResultList -> zoom resultList $ handleListEvent e
        Just CategorySelection -> zoom categorySel $ handleListEvent e
        _ -> pure ()
appHandleEventMain _ = pure ()

appCursor :: TpbSt -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = focusRingCursor (^. fRing)

appMap :: AttrMap
appMap =
    attrMap
        defAttr
        [ (listSelectedFocusedAttr, black `on` brightYellow)
        , (editFocusedAttr, fg brightWhite)
        , (listSelectedAttr, fg brightWhite)
        , (dialogAttr, fg brightWhite)
        , (buttonAttr, black `on` brightWhite)
        , (buttonSelectedAttr, bg brightYellow)
        , (attrResults, fg brightYellow)
        , (attrCats, fg brightBlue)
        , (attrKey, withStyle (fg brightYellow) bold)
        , (attrBold, withStyle (fg brightWhite) bold)
        ]

attrKey :: AttrName
attrKey = attrName "key"

attrBold :: AttrName
attrBold = attrName "bold"

attrResults :: AttrName
attrResults = attrName "results"

attrCats :: AttrName
attrCats = attrName "categories"

theApp :: App TpbSt e Name
theApp =
    App
        { appDraw = ui
        , appChooseCursor = appCursor
        , appHandleEvent = appEvents
        , appStartEvent = pure ()
        , appAttrMap = const appMap
        }

main :: IO ()
main = void $ defaultMain theApp initState
