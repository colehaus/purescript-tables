module Data.Table.Internal where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable, hash)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Tuple as Tuple
import Partial.Unsafe (unsafePartialBecause)

newtype Table rowId columnId cell
  = MkTable
  { cells :: HashMap (Tuple rowId columnId) cell
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
instance hashableMissingCell ::
  (Hashable rowId, Hashable columnId) =>
  Hashable (MissingCell rowId columnId) where
  hash (MkMissingCell t) = hash t

valid ::
  forall rowId columnId cell.
  Hashable rowId => Hashable columnId =>
  Table rowId columnId cell -> Either (HashSet (MissingCell rowId columnId)) Unit
valid (MkTable { cells }) =
  if missingCells /= HashSet.empty
  then Left (HashSet.map MkMissingCell missingCells)
  else Right unit
  where
    missingCells = HashSet.fromArray combos `HashSet.difference` keys
    combosSet = HashSet.fromArray combos
    combos = do
      rId <- HashSet.toArray $ rowIds
      cId <- HashSet.toArray $ colIds
      pure (Tuple rId cId)
    rowIds = HashSet.map Tuple.fst keys
    colIds = HashSet.map Tuple.snd keys
    keys = HashSet.fromArray <<< HashMap.keys $ cells

mk ::
  forall cell rowId columnId.
  Hashable rowId => Hashable columnId =>
  HashMap (Tuple rowId columnId) cell ->
  Either (HashSet (MissingCell rowId columnId)) (Table rowId columnId cell)
mk cells = table <$ valid table
  where
    table = MkTable { cells }

vectors ::
  forall id cell rowId columnId.
  Hashable id => Hashable rowId => Hashable columnId => Hashable cell =>
  (Tuple rowId columnId -> id) ->
  Table rowId columnId cell -> HashSet (HashMap (Tuple rowId columnId) cell)
vectors proj (MkTable { cells }) =
  HashSet.fromArray <<< HashMap.values <<<
  unions <<< map munge <<<
  (identity :: forall a. Array a -> Array a) <<<
  HashMap.toArrayBy Tuple $
  cells
  where
    unions = foldr (HashMap.unionWith (<>)) HashMap.empty
    munge cell = HashMap.singleton (proj $ fst cell) $ uncurry HashMap.singleton cell

vectors' ::
  forall id idr cell rowId columnId.
  Hashable id => Hashable idr =>
  (Tuple rowId columnId -> id) -> (Tuple rowId columnId -> idr) ->
  Table rowId columnId cell -> HashMap id (HashMap idr cell)
vectors' proj projR (MkTable { cells }) =
  unions <<< map munge <<<
  (identity :: forall a. Array a -> Array a) <<<
  HashMap.toArrayBy Tuple $
  cells
  where
    unions = foldr (HashMap.unionWith (<>)) HashMap.empty
    munge (Tuple id cell) =
      HashMap.singleton (proj id) $ HashMap.singleton (projR id) cell

vector ::
  forall id rid rowId columnId cell.
  Eq id => Hashable rid =>
  (Tuple rowId columnId -> id) ->
  (Tuple rowId columnId -> rid) ->
  Table rowId columnId cell -> id -> HashMap rid cell
vector proj projR (MkTable { cells }) id =
  HashMap.fromArray <<< map (first projR) <<< HashMap.toArrayBy Tuple <<<
  HashMap.filterKeys (\c -> proj c == id) $
  cells
  where
    first f (Tuple a b) = Tuple (f a) b

-- | The mapping function should preserve the length of the list. If it doesn't, you'll end up
-- with a `Left`.
mapVectors ::
  forall rowId columnId cell1 cell2 id idr.
  Hashable id => Hashable rowId => Hashable columnId => Hashable cell1 => Hashable idr =>
  (Tuple rowId columnId -> id) ->
  (Tuple rowId columnId -> idr) ->
  (id -> idr -> Tuple rowId columnId) ->
  (HashMap idr cell1 -> HashMap idr cell2) ->
  Table rowId columnId cell1 ->
  Either (HashSet (MissingCell rowId columnId)) (Table rowId columnId cell2)
mapVectors proj projR comb f tbl = mk newCells
  where
    newCells =
      HashMap.fromArray <<< join <<<
      map (lift f) <<< Array.fromFoldable <<< vectors proj $
      tbl
    lift g vec =
      map (first $ comb id) <<< HashMap.toArrayBy Tuple <<<
      g <<<
      HashMap.fromArray <<< map (first projR) <<< HashMap.toArrayBy Tuple $
      vec
      where
        id =
          proj <<<
          unsafePartialBecause "Grouping guarantees non-empty on inner collection" $
          fromJust <<< Array.head <<< HashMap.keys $
          vec
    first :: forall x y z. (x -> z) -> Tuple x y -> Tuple z y
    first g (Tuple a b) = Tuple (g a) b
