module Main where 

import Test.Tasty.HUnit
import Test.Tasty
import Formula
import SF
import CNF
import DNF
import NNF

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tests_simplify, tests_nnf]

tests_simplify :: TestTree
tests_simplify = testGroup "simplify" 
    [ 
        testCase "atom" $ simplify p1 @?= p1,
        testCase "not" $ simplify (Not p1) @?= Not p1,
        testCase "arrows" $ simplify (p1 :-> p2 :-> p3) @?= (Not p1) `Or` (Not p2) `Or` p3,
        testCase "EQ + arrow" $ simplify ((p1 :<->: p2) :-> p3) @?= ((Not (( (Not p1) `Or` p2 ) `And` ((Not p2) `Or` p1))) `Or` p3),
        testCase "AND + EQ" $ simplify ((p1 `And` p2) :<->: p1) @?= ((Not (p1 `And` p2)) `Or` p1) `And` ((Not p1) `Or` (p1 `And` p2))
    ]

tests_nnf :: TestTree
tests_nnf = testGroup "NNF"
    [
        testCase "De Morgan's + (NOT) inside OR" $ toNNF (Not ((Not p1) `Or` (Not p2))) @?= p1 `And` p2,
        testCase "De Morgan's + AND" $ toNNF (Not (p1 `And` (Not p2))) @?= (Not p1) `Or` p2,
        testCase "Three agrs OR" $ toNNF (Not (p1 `Or` p2 `Or` p3)) @?= (Not p1) `And` (Not p2) `And` (Not p3),
        testCase "Double NOT + two brackets" $ toNNF ((Not (p1 `And` p2)) `Or` (Not (Not p3))) @?= ((Not p1) `Or` (Not p2)) `Or` p3,
        testCase "So much NOT's even" $ toNNF (Not (Not (Not (Not p1)))) @?= p1, 
        testCase "So much NOT's odd" $ toNNF (Not (Not (Not (Not (Not p1))))) @?= (Not p1)
    ]

p1 = (Atom "p1")
p2 = (Atom "p2")
p3 = (Atom "p3")

sampleOr = (Atom "p1") `Or` (Atom "p2")
sample1 = Not (p1 `Or` p2)
sample3 = Not (p1 `And` p2) `And` p3
sample4 = p1 :-> p2 `And` p3