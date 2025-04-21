module Main (main) where

import Test.Hspec
import FlowChart.Spec
import TuringMachine.Spec
import MixHelpers.Spec
import Mix.Spec
import Futamura.Spec

main :: IO ()
main = do
    hspec spec
    hspec specTM
    hspec specDivision
    hspec specLookup
    hspec specElem
    hspec specInsert
    hspec specReduce
    hspec specToFromPrgrm
    hspec specMix
    hspec specIsStatic
    hspec specList
    hspec specFutamura1