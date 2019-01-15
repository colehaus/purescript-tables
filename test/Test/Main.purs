module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
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
        Table.mk Just Just (Map.fromFoldable cells) `shouldSatisfy` Either.isRight
    describe "multi-celled tables" do
      let mkVec (NonEmptyList (NonEmpty c1 (c2 : Nil))) = Just (Tuple c1 c2)
          mkVec _ = Nothing
      it "can construct them" do
        let cells =
              [ Tuple (Tuple "row1" "column1") "cell1"
              , Tuple (Tuple "row1" "column2") "cell2"
              , Tuple (Tuple "row2" "column1") "cell3"
              , Tuple (Tuple "row2" "column2") "cell4"
              ]
        Table.mk mkVec mkVec (Map.fromFoldable cells) `shouldSatisfy` Either.isRight
      it "rejects bad ones" do
        let cells =
              [ Tuple (Tuple "row1" "column1") "cell1"
              ]
        Table.mk mkVec mkVec (Map.fromFoldable cells) `shouldSatisfy` Either.isLeft
      it "uses the right ordering" do
        let cells =
              [ Tuple (Tuple "row1" "column1") "cell4"
              , Tuple (Tuple "row1" "column2") "cell3"
              , Tuple (Tuple "row2" "column1") "cell2"
              , Tuple (Tuple "row2" "column2") "cell1"
              ]
            table = unsafePartial $ case Table.mk mkVec mkVec (Map.fromFoldable cells) of
              Right t -> t
        Table.row table "row1" `shouldEqual` Just (Tuple "cell4" "cell3")
        Table.row table "row2" `shouldEqual` Just (Tuple "cell2" "cell1")
        Table.column table "column1" `shouldEqual` Just (Tuple "cell4" "cell2")
        Table.column table "column2" `shouldEqual` Just (Tuple "cell3" "cell1")
