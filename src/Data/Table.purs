module Data.Table
  ( module Data.Table
  , module ForReExport
  ) where

import Prelude

import Data.Either (Either)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple

import Data.Table.Internal
import Data.Table.Internal (Table, Error) as ForReExport


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

mk ::
  forall cell row column rowId columnId.
  Ord rowId => Ord columnId => Ord cell =>
  (NonEmptyList cell -> Maybe row) ->
  (NonEmptyList cell -> Maybe column) ->
  Map (Tuple rowId columnId) cell ->
  Either (Set (Error rowId columnId cell)) (Table rowId columnId cell row column)
mk mkRow mkColumn cells = table <$ valid table
  where
    table = MkTable { cells, mkRow, mkColumn }

rowIds ::
  forall cell row column rowId columnId.
  Ord rowId =>
  Table rowId columnId cell row column -> Set rowId
rowIds (MkTable { cells }) = Set.map Tuple.fst <<< Map.keys $ cells

columnIds ::
  forall cell row column rowId columnId.
  Ord columnId =>
  Table rowId columnId cell row column -> Set columnId
columnIds (MkTable { cells }) = Set.map Tuple.snd <<< Map.keys $ cells
