module Data.Table.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map as Map
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
  forall id c rowId columnId.
  Ord id =>
  (Tuple rowId columnId -> id) ->
  Table rowId columnId c -> List (NonEmptyList (Tuple (Tuple rowId columnId) c))
vectors proj (MkTable { cells }) =
  Map.values <<<
  Map.fromFoldableWith (<>) <<< map munge <<<
  (identity :: forall a. Array a -> Array a) <<<
  Map.toUnfoldableUnordered $
  cells
  where
    munge cell = Tuple (proj $ fst cell) $ NonEmpty.singleton cell

vectors' ::
  forall id idr c rowId columnId.
  Ord id =>
  (Tuple rowId columnId -> id) -> (Tuple rowId columnId -> idr) ->
  Table rowId columnId c -> List (Tuple id (NonEmptyList (Tuple idr c)))
vectors' proj projR (MkTable { cells }) =
  Map.toUnfoldable <<<
  Map.fromFoldableWith (<>) <<< map munge <<<
  (identity :: forall a. Array a -> Array a) <<<
  Map.toUnfoldableUnordered $
  cells
  where
    munge (Tuple id cell) = Tuple (proj id) $ NonEmpty.singleton (Tuple (projR id) cell)

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
  forall rowId columnId cell1 cell2 id.
  Ord id => Ord rowId => Ord columnId =>
  (Tuple rowId columnId -> id) ->
  (NonEmptyList cell1 -> NonEmptyList cell2) ->
  Table rowId columnId cell1 ->
  Either (Set (MissingCell rowId columnId)) (Table rowId columnId cell2)
mapVectors proj f tbl = mk newCells
  where
    newCells =
      Map.fromFoldable <<< (NonEmpty.toList =<< _) <<<
      map (lift f) <<< vectors proj $ tbl
    lift g col = NonEmpty.zip (fst <$> col) (g $ snd <$> col)
