module Main (main) where

import Test.Hspec
import FlowChart.Spec
import TuringMachine.Spec
import Division.Spec

main :: IO ()
main = do
    hspec spec
    hspec specTM
    hspec specDivision