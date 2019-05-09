module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Data.Table as Table

main :: Effect Unit
main = run [consoleReporter] do
  describe "Core type" do
    it "can construct single-cell tables" do
        let cells =
              [ Tuple (Tuple "row1" "column1") "cell1"
              ]
        Table.mk (Map.fromFoldable cells) `shouldSatisfy` Either.isRight
    describe "multi-celled tables" do
      it "can construct them" do
        let cells =
              [ Tuple (Tuple "row1" "column1") "cell1"
              , Tuple (Tuple "row1" "column2") "cell2"
              , Tuple (Tuple "row2" "column1") "cell3"
              , Tuple (Tuple "row2" "column2") "cell4"
              ]
        Table.mk (Map.fromFoldable cells) `shouldSatisfy` Either.isRight
      it "rejects bad ones" do
        let cells =
              [ Tuple (Tuple "row1" "column1") "cell1"
              , Tuple (Tuple "row2" "column2") "cell2"
              ]
        Table.mk (Map.fromFoldable cells) `shouldSatisfy` Either.isLeft
      it "uses the right ordering" do
        let cells =
              [ Tuple (Tuple "row1" "column1") "cell4"
              , Tuple (Tuple "row1" "column2") "cell3"
              , Tuple (Tuple "row2" "column1") "cell2"
              , Tuple (Tuple "row2" "column2") "cell1"
              ]
            table = unsafePartial $ case Table.mk (Map.fromFoldable cells) of
              Right t -> t
        Table.row table "row1" `shouldEqual` ("cell4" : "cell3" : Nil)
        Table.row table "row2" `shouldEqual` ("cell2" : "cell1" : Nil)
        Table.column table "column1" `shouldEqual` ("cell4" : "cell2" : Nil)
        Table.column table "column2" `shouldEqual` ("cell3" : "cell1" : Nil)
  describe "Mapping" do
    it "can work with simple functions" do
        let cellsBefore =
              [ Tuple (Tuple "row1" "column1") 1
              , Tuple (Tuple "row1" "column2") 2
              , Tuple (Tuple "row2" "column1") 3
              , Tuple (Tuple "row2" "column2") 4
              ]
            tableBefore =
              unsafePartial $ Either.fromRight $
              Table.mk (Map.fromFoldable cellsBefore)
            cellsAfter =
              [ Tuple (Tuple "row1" "column1") 2
              , Tuple (Tuple "row1" "column2") 3
              , Tuple (Tuple "row2" "column1") 4
              , Tuple (Tuple "row2" "column2") 5
              ]
            tableAfter =
              unsafePartial $ Either.fromRight $
              Table.mk (Map.fromFoldable cellsAfter)
        Table.mapColumns (map (_ + 1)) tableBefore `shouldEqual` Right tableAfter
