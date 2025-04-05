module Main (main) where

import Test.Hspec
import FlowChart.Spec
import TuringMachine.Spec
import MixHelpers.Spec

main :: IO ()
main = do
    hspec spec
    hspec specTM
    hspec specDivision