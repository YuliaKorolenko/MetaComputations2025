module Mix.Spec where

import Test.Hspec

import Dsl
import Ast
import Mix


maxProgramWithConditions :: Program
maxProgramWithConditions = program ["a", "b", "c"]
    [bl "initial" ["a" #= v "b"],
    bl "tail_and_head_a" [ "tail_a" #= tl (v "a"), "head_a" #= hd (v "a")],
    bl "tail_and_head_b" ["tail_b" #= tl (v "b"), "head_b" #= hd (v "b")],
    bl "tail_and_head_c" ["tail_c" #= tl (v "c"), "head_c" #= hd (v "tail_a")]]

abStatic :: Constant
abStatic = ListC [ListC [ExprC $ EVar $ VarName "a", lInt [1, 2, 3]],
                  ListC [ExprC $ EVar $ VarName "b", lInt [5, 6, 8, 9]]]

specMix :: Spec
specMix = do 
    describe "1" $ do
        it "2" $ do
            print 2
            -- let staticV = generateStaticVars maxProgramWithConditions abStatic
            -- undefined



