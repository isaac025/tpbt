{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick.AttrMap (AttrMap, attrMap)
import Brick.Focus (FocusRing, focusNext, focusPrev, focusRing, focusRingCursor)
import Brick.Main (App (..), halt)
import Brick.Types (BrickEvent (..), CursorLocation, EventM, Widget)
import Brick.Widgets.Core (str, (<+>), (<=>))
import Brick.Widgets.Edit (Editor, editor, renderEditor)
import Brick.Widgets.List (GenericList, list)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events (Event (..), Key (..))
import Lens.Micro ((^.))
import Lens.Micro.Mtl ((%=))
import Lens.Micro.TH (makeLenses)
import TPB.Types

data Name
    = SearchBar
    | CategorySelection
    | ResultList
    deriving (Ord, Eq, Show)

data TpbSt = TpbSt
    { _results :: [Result]
    , _searchBar :: Editor String Name
    , _resultList :: GenericList Name [] Result
    , _categorySel :: GenericList Name [] Audio
    , _fRing :: FocusRing Name
    }
makeLenses ''TpbSt

initState :: TpbSt
initState =
    TpbSt
        { _results = []
        , _searchBar = editor SearchBar (Just 1) ""
        , _resultList = list ResultList [] 1
        , _categorySel = list CategorySelection [Music .. Other] 1
        , _fRing = focusRing [SearchBar, CategorySelection, ResultList]
        }

ui :: TpbSt -> [Widget Name]
ui tpbst = [searchBarUI <+> resultsListUI <=> categoryUI]
  where
    searchBarUI :: Widget Name
    searchBarUI = label "Search: " <+> searchBarEditor
      where
        label = str
        searchBarEditor = renderEditor (str . unlines) True (tpbst ^. searchBar)

    resultsListUI :: Widget Name
    resultsListUI = undefined

    categoryUI :: Widget Name
    categoryUI = undefined

appEvents :: BrickEvent Name e -> EventM Name TpbSt ()
appEvents (VtyEvent (EvKey KEsc [])) = halt
appEvents (VtyEvent (EvKey (KChar 'q') [])) = halt
appEvents (VtyEvent (EvKey (KChar '\t') [])) = fRing %= focusNext
appEvents (VtyEvent (EvKey KBackTab [])) = fRing %= focusPrev
appEvents _ = pure ()

appCursor :: TpbSt -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = focusRingCursor (^. fRing)

appMap :: AttrMap
appMap = attrMap defAttr []

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
main = undefined
