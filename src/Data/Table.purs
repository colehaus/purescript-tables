module Data.Table
  ( module Data.Table
  , module ForReExport
  ) where

import Data.Table.Internal
import Prelude

import Data.Either (Either)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Data.Table.Internal (Table, MissingCell, mk) as ForReExport
import Data.Tuple (Tuple(..), fst, snd)


cell ::
  forall rowId columnId cell.
  Hashable columnId => Hashable rowId =>
  Table rowId columnId cell -> rowId -> columnId -> Maybe cell
cell (MkTable { cells }) rowId columnId = Tuple rowId columnId `HashMap.lookup` cells

row ::
  forall rowId columnId cell.
  Hashable rowId => Hashable columnId => Hashable cell =>
  Table rowId columnId cell -> rowId -> HashMap columnId cell
row tbl = vector fst snd tbl

column ::
  forall rowId columnId cell.
  Hashable columnId => Hashable rowId => Hashable cell =>
  Table rowId columnId cell -> columnId -> HashMap rowId cell
column tbl = vector snd fst tbl

rowIds ::
  forall cell rowId columnId.
  Hashable rowId =>
  Table rowId columnId cell -> HashSet rowId
rowIds (MkTable { cells }) = HashSet.fromArray <<< map fst <<< HashMap.keys $ cells

columnIds ::
  forall cell rowId columnId.
  Hashable columnId =>
  Table rowId columnId cell -> HashSet columnId
columnIds (MkTable { cells }) = HashSet.fromArray <<< map snd <<< HashMap.keys $ cells

-- | The mapping function should preserve the length of the list. If it doesn't, you'll end up
-- with a `Left`.
mapColumns ::
  forall cell2 cell1 columnId rowId.
  Hashable columnId => Hashable rowId => Hashable cell1 =>
  (HashMap rowId cell1 -> HashMap rowId cell2) ->
  Table rowId columnId cell1 ->
  Either (HashSet (MissingCell rowId columnId)) (Table rowId columnId cell2)
mapColumns = mapVectors snd fst (flip Tuple)

-- | The mapping function should preserve the length of the list. If it doesn't, you'll end up
-- with a `Left`.
mapRows ::
  forall cell2 cell1 columnId rowId.
  Hashable columnId => Hashable rowId => Hashable cell1 =>
  (HashMap columnId cell1 -> HashMap columnId cell2) ->
  Table rowId columnId cell1 ->
  Either (HashSet (MissingCell rowId columnId)) (Table rowId columnId cell2)
mapRows = mapVectors fst snd Tuple

columns ::
  forall idr idc c.
  Hashable idc => Hashable idr => Hashable c =>
  Table idr idc c -> HashSet (HashMap (Tuple idr idc) c)
columns = vectors snd

columns' ::
  forall idr idc c.
  Hashable idc => Hashable idr =>
  Table idr idc c -> HashMap idc (HashMap idr c)
columns' = vectors' snd fst

rows ::
  forall idr idc cell.
  Hashable idr => Hashable idc => Hashable cell =>
  Table idr idc cell -> HashSet (HashMap (Tuple idr idc) cell)
rows = vectors fst

rows' ::
  forall idr idc c.
  Hashable idr => Hashable idc =>
  Table idr idc c -> HashMap idr (HashMap idc c)
rows' = vectors' fst snd
