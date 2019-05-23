module Data.Table.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple as Tuple

newtype Table rowId columnId cell
  = MkTable
  { cells :: Map (Tuple rowId columnId) cell
  }
derive instance genericTable :: Generic (Table rowId columnId cell) _
derive instance eqTable ::
  (Eq rowId, Eq columnId, Eq cell) =>
  Eq (Table rowId columnId cell)
instance showTable ::
  (Show rowId, Show columnId, Show cell) =>
  Show (Table rowId columnId cell) where
  show = genericShow

newtype MissingCell rowId columnId = MkMissingCell (Tuple rowId columnId)
derive instance genericMissingCell :: Generic (MissingCell rowId columnId) _
derive instance eqMissingCell ::
  (Eq rowId, Eq columnId) =>
  Eq (MissingCell rowId columnId)
derive instance ordMissingCell ::
  (Ord rowId, Ord columnId) =>
  Ord (MissingCell rowId columnId)
instance showMissingCell ::
  (Show rowId, Show columnId) =>
  Show (MissingCell rowId columnId) where
  show = genericShow

valid ::
  forall rowId columnId cell.
  Ord rowId => Ord columnId =>
  Table rowId columnId cell -> Either (Set (MissingCell rowId columnId)) Unit
valid (MkTable { cells }) =
  if missingCells /= Set.empty
  then Left (Set.map MkMissingCell missingCells)
  else Right unit
  where
    missingCells = Set.fromFoldable combos `Set.difference` keys
    combosSet = Set.fromFoldable combos
    combos :: List (Tuple rowId columnId)
    combos = do
      rId <- Set.toUnfoldable rowIds
      cId <- Set.toUnfoldable colIds
      pure (Tuple rId cId)
    rowIds = Set.map Tuple.fst keys
    colIds = Set.map Tuple.snd keys
    keys = Map.keys cells

mk ::
  forall cell rowId columnId.
  Ord rowId => Ord columnId =>
  Map (Tuple rowId columnId) cell ->
  Either (Set (MissingCell rowId columnId)) (Table rowId columnId cell)
mk cells = table <$ valid table
  where
    table = MkTable { cells }

vectors ::
  forall ide idc c rowId columnId.
  Eq ide => Ord idc =>
  (Tuple rowId columnId -> ide) -> (Tuple rowId columnId -> idc) ->
  Table rowId columnId c -> List (NonEmptyList (Tuple (Tuple rowId columnId) c))
vectors projEq projComp (MkTable { cells }) =
  List.groupBy ((==) `on` (projEq <<< fst)) <<<
  List.sortBy (compare `on` (projComp <<< fst)) <<<
  Map.toUnfoldable $
  cells

vectors' ::
  forall ide idc idr c rowId columnId.
  Eq ide => Ord idc =>
  (Tuple rowId columnId -> ide) -> (Tuple rowId columnId -> idc) ->
  (Tuple rowId columnId -> idr) ->
  Table rowId columnId c -> List (Tuple ide (NonEmptyList (Tuple idr c)))
vectors' projEq projComp projRest (MkTable { cells }) =
  map liftId <<<
  List.groupBy ((==) `on` (projEq <<< fst)) <<<
  List.sortBy (compare `on` (projComp <<< fst)) <<<
  Map.toUnfoldable $
  cells
  where
    liftId xs@(NonEmptyList (NonEmpty (Tuple id c) _)) =
      Tuple (projEq id) (first projRest <$> xs)
    first f (Tuple a b) = Tuple (f a) b

vector ::
  forall id rowId columnId cell.
  Eq id =>
  (Tuple rowId columnId -> id) ->
  Table rowId columnId cell -> id -> List cell
vector proj (MkTable { cells }) id =
  map Tuple.snd <<<
  List.filter (\c -> proj (fst c) == id) <<<
  Map.toUnfoldable $
  cells

-- | The mapping function should preserve the length of the list. If it doesn't, you'll end up with a `Left`.
mapVectors ::
  forall rowId columnId cell1 cell2 ide idc.
  Eq ide => Ord idc => Ord rowId => Ord columnId =>
  (Tuple rowId columnId -> ide) -> (Tuple rowId columnId -> idc) ->
  (NonEmptyList cell1 -> NonEmptyList cell2) ->
  Table rowId columnId cell1 ->
  Either (Set (MissingCell rowId columnId)) (Table rowId columnId cell2)
mapVectors projEq projComp f tbl = mk newCells
  where
    newCells = Map.fromFoldable <<< (NonEmpty.toList =<< _) <<< map (lift f) <<< vectors projEq projComp $ tbl
    lift g col = NonEmpty.zip (fst <$> col) (g $ snd <$> col)
