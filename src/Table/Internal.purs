module Table.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Tuple (Tuple)
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
    badColumns =
      map (\cs -> MkBadColumn (Tuple.snd <<< Tuple.fst <<< NEList.head $ cs) (Tuple.snd <$> cs)) <<<
      List.filter (Maybe.isNothing <<< mkColumn <<< map Tuple.snd) $ columns
    badRows =
      map (\cs -> MkBadRow (Tuple.fst <<< Tuple.fst <<< NEList.head $ cs) (Tuple.snd <$> cs)) <<<
      List.filter (Maybe.isNothing <<< mkRow <<< map Tuple.snd) $ rows
    columns = List.groupBy ((==) `on` (Tuple.snd <<< Tuple.fst)) <<< List.sortBy (compare `on` (Tuple.swap <<< Tuple.fst)) <<< Map.toUnfoldable $ cells
    rows = List.groupBy ((==) `on` (Tuple.fst <<< Tuple.fst)) <<< List.sortBy (compare `on` Tuple.fst) <<< Map.toUnfoldable $ cells
