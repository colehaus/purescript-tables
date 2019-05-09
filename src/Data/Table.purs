module Data.Table
  ( module Data.Table
  , module ForReExport
  ) where

import Data.Table.Internal
import Prelude

import Data.Either (Either)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Table.Internal (Table, MissingCell, mk) as ForReExport
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple as Tuple


cell ::
  forall rowId columnId cell.
  Ord columnId => Ord rowId =>
  Table rowId columnId cell -> rowId -> columnId -> Maybe cell
cell (MkTable { cells }) rowId columnId = Tuple rowId columnId `Map.lookup` cells

row ::
  forall rowId columnId cell.
  Eq rowId =>
  Table rowId columnId cell -> rowId -> List cell
row tbl = vector fst tbl

column ::
  forall rowId columnId cell.
  Eq columnId =>
  Table rowId columnId cell -> columnId -> List cell
column tbl = vector snd tbl

rowIds ::
  forall cell rowId columnId.
  Ord rowId =>
  Table rowId columnId cell -> Set rowId
rowIds (MkTable { cells }) = Set.map Tuple.fst <<< Map.keys $ cells

columnIds ::
  forall cell rowId columnId.
  Ord columnId =>
  Table rowId columnId cell -> Set columnId
columnIds (MkTable { cells }) = Set.map Tuple.snd <<< Map.keys $ cells

-- | The mapping function should preserve the length of the list. If it doesn't, you'll end up with a `Left`.
mapColumns ::
  forall cell2 cell1 columnId rowId.
  Ord columnId => Ord rowId =>
  (NonEmptyList cell1 -> NonEmptyList cell2) ->
  Table rowId columnId cell1 ->
  Either (Set (MissingCell rowId columnId)) (Table rowId columnId cell2)
mapColumns = mapVectors snd Tuple.swap

-- | The mapping function should preserve the length of the list. If it doesn't, you'll end up with a `Left`.
mapRows ::
  forall cell2 cell1 columnId rowId.
  Ord columnId => Ord rowId =>
  (NonEmptyList cell1 -> NonEmptyList cell2) ->
  Table rowId columnId cell1 ->
  Either (Set (MissingCell rowId columnId)) (Table rowId columnId cell2)
mapRows = mapVectors fst identity

columns ::
  forall idr idc c.
  Ord idc => Ord idr =>
  Table idr idc c -> List (NonEmptyList (Tuple (Tuple idr idc) c))
columns = vectors snd Tuple.swap

rows ::
  forall idr idc c.
  Ord idc => Ord idr =>
  Table idr idc c -> List (NonEmptyList (Tuple (Tuple idr idc) c))
rows = vectors fst identity
