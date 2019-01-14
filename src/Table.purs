module Table where

import Prelude

import Data.Either (Either)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Set (Set)

import Table.Internal

cell ::
  forall rowId columnId cell row column.
  Ord columnId => Ord rowId =>
  Table rowId columnId cell row column -> rowId -> columnId -> Maybe cell
cell (MkTable { cells }) rowId columnId = Tuple rowId columnId `Map.lookup` cells

row ::
  forall rowId columnId cell row column.
  Eq rowId =>
  Table rowId columnId cell row column -> rowId -> Maybe row
row (MkTable { cells, mkRow }) = vector rowId mkRow cells

column ::
  forall rowId columnId cell row column.
  Eq columnId =>
  Table rowId columnId cell row column -> columnId -> Maybe column
column (MkTable { cells, mkColumn }) = vector columnId mkColumn cells

mkTable ::
  forall cell row column rowId columnId.
  Ord rowId => Ord columnId => Ord cell =>
  (NonEmptyList cell -> Maybe row) ->
  (NonEmptyList cell -> Maybe column) ->
  Map (Tuple rowId columnId) cell ->
  Either (Set (Error rowId columnId cell)) (Table rowId columnId cell row column)
mkTable mkRow mkColumn cells = table <$ valid table
  where
    table = MkTable { cells, mkRow, mkColumn }
