module Table.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Set (Set)
import Data.Set as Set

data Table rowId columnId cell row column
  = MkTable
  { cells :: Map (Tuple rowId columnId) cell
  , mkRow :: NonEmptyList cell -> Maybe row
  , mkColumn :: NonEmptyList cell -> Maybe column
  }
instance showTable ::
  (Show cell, Show columnId, Show rowId) =>
  Show (Table rowId columnId cell row column) where
  show (MkTable { cells }) = "MkTable (" <> show cells <> ")"

data Error rowId columnId cell
  = MkBadRow rowId (NonEmptyList cell)
  | MkBadColumn columnId (NonEmptyList cell)
derive instance genericError :: Generic (Error rowId columnId cell) _
derive instance eqError ::
  (Eq cell, Eq columnId, Eq rowId) =>
  Eq (Error rowId columnId cell)
derive instance ordError ::
  (Ord cell, Ord columnId, Ord rowId) =>
  Ord (Error rowId columnId cell)
instance showError ::
  (Show cell, Show columnId, Show rowId) =>
  Show (Error rowId columnId cell) where
  show = genericShow

valid ::
  forall rowId columnId cell row column.
  Ord rowId => Ord columnId => Ord cell =>
  Table rowId columnId cell row column -> Either (Set (Error rowId columnId cell)) Unit
valid (MkTable { cells, mkColumn, mkRow }) =
  if List.null badColumns && List.null badRows
  then Right unit
  else Left <<< Set.fromFoldable $ badColumns <> badRows
  where
    badColumns = bad MkBadColumn columnId mkColumn $ vectors columnId columnSort cells
    badRows = bad MkBadRow rowId mkRow $ vectors rowId rowSort cells
    bad ::
      forall e ide c id v.
      (ide -> NonEmptyList c -> e) ->
      (Tuple id c -> ide) ->
      (NonEmptyList c -> Maybe v) ->
      List (NonEmptyList (Tuple id c)) ->
      List e
    bad mkErr proj mkVec =
      map (\cs -> mkErr (proj <<< NEList.head $ cs) (Tuple.snd <$> cs)) <<<
      List.filter (Maybe.isNothing <<< mkVec <<< map Tuple.snd)

vectors ::
  forall ide idc c id.
  Eq ide => Ord idc =>
  (Tuple id c -> ide) -> (Tuple id c -> idc) -> Map id c -> List (NonEmptyList (Tuple id c))
vectors projEq projComp cells =
  List.groupBy ((==) `on` projEq) <<<
  List.sortBy (compare `on` projComp) <<<
  Map.toUnfoldable $
  cells

columnId :: forall r c a. Tuple (Tuple r c) a -> c
columnId (Tuple (Tuple _ c) _) = c

columnSort :: forall r c a. Tuple (Tuple r c) a -> Tuple c r
columnSort (Tuple id _) = Tuple.swap id

rowId :: forall r c a. Tuple (Tuple r c) a -> r
rowId (Tuple (Tuple r _) _) = r

rowSort :: forall r c a. Tuple (Tuple r c) a -> Tuple r c
rowSort (Tuple id _) = id

vector ::
  forall id vec rowId columnId cell.
  Eq id =>
  (Tuple (Tuple rowId columnId) cell -> id) ->
  (NonEmptyList cell -> Maybe vec) ->
  Map (Tuple rowId columnId) cell -> id -> Maybe vec
vector proj mk cells id =
  (mk <=< NEList.fromFoldable) <<<
  map Tuple.snd <<<
  List.filter (\c -> proj c == id) <<<
  Map.toUnfoldable $
  cells

