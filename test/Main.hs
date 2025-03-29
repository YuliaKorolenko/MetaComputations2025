module Main (main) where

import Test.Hspec
import FlowChart.Spec
import TuringMachine.Spec

main :: IO ()
main = do
    hspec spec
    hspec specTM