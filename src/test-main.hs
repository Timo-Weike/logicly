module Main where

import Test.Hspec
import Text.Printf
import Control.Monad

import System.Exit (exitFailure)
import LogicExp

-- main = do
--     putStrLn "This test always fails!"
--     exitFailure

validate exs =
    forM_ exs $ \(input, output) ->
        it (printf "should return %s given %s as input" (show output) (show input)) $ do
            let res = parse input
            case res of 
                Left s     -> error s
                Right exp -> toString exp `shouldBe` output
                
main = hspec $ do
    describe "text" $ do
        let exs = 
                [
                      ("~a", "~a")
                    , ("a | b & c & d", "(a | ((b & c) & d))")
                    , ("~~a", "~~a")
                    , ("a v b -> b and c", "((a | b) -> (b & c))")
                    , ("a or (b & c & d)", "(a | ((b & c) & d))")
                    , ("a & (~d)", "(a & ~d)")
                    , ("a & (b & ~d)", "(a & (b & ~d))")
                    , ("a -> ~~a & ~(b v ~c & d)", "(a -> (~~a & ~(b | (~c & d))))")
                ]
        validate exs
    describe "prioritys" $ do
        let exs =
                [
                    ("a & ~b","(a & ~b)") -- not before and
                    , ("a | ~b", "(a | ~b)") -- not before or
                    , ("a x   ~b", "(a x ~b)") -- not before xor
                    , ("a ->  ~b", "(a -> ~b)") -- not before ->
                    , ("a <-> ~b", "(a <-> ~b)") -- not before <->
                    , ("~a & b", "(~a & b)") -- not before and
                    , ("~a | b", "(~a | b)") -- not before or
                    , ("~a x   b", "(~a x b)") -- not before xor
                    , ("~a ->  b", "(~a -> b)") -- not before ->
                    , ("~a <-> b", "(~a <-> b)") -- not before <->

                    , ("a or b and c", "(a | (b & c))") -- and before or
                    , ("a + b and c", "(a x (b & c))") -- and before xor
                    , ("a -> b and c", "(a -> (b & c))") -- and before ->
                    , ("a <-> b and c", "(a <-> (b & c))") -- and before <->
                    , ("b and c or a", "((b & c) | a)") -- and before or
                    , ("b and c xor a", "((b & c) x a)") -- and before xor
                    , ("b and c -> a", "((b & c) -> a)") -- and before ->
                    , ("b and c <-> a", "((b & c) <-> a)") -- and before <->
                    
                    , ("a xor b or c", "(a x (b | c))") -- or before xor
                    , ("a ->  b or c", "(a -> (b | c))") -- or before ->
                    , ("a <-> b or c", "(a <-> (b | c))") -- or before <->
                    , ("b or c xor a", "((b | c) x a)") -- or before xor
                    , ("b or c ->  a", "((b | c) -> a)") -- or before ->
                    , ("b or c <-> a", "((b | c) <-> a)") -- or before <->

                    , ("a ->  b xor c", "(a -> (b x c))") -- xor before ->
                    , ("a <-> b xor c", "(a <-> (b x c))") -- xor before <->
                    , ("b xor c -> a", "((b x c) -> a)") -- xor before ->
                    , ("b xor c <-> a", "((b x c) <-> a)") -- xor before <->

                    , ("a <-> b -> c", "(a <-> (b -> c))") -- -> before <->
                    , ("b -> c <-> a", "((b -> c) <-> a)") -- -> before <->

                ]
        validate exs