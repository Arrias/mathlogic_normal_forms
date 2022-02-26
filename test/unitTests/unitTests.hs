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
tests = testGroup "Tests" [tests_simplify, tests_NNF, tests_DNF, tests_Parser]

p1 = (Atom "p1")
p2 = (Atom "p2")
p3 = (Atom "p3")

simpleOr = p1 `Or` p2
simpleAnd = p1 `Or` p2
simpleHence = p1 :-> p2 
simpleEq = p1 :<->: p2 

tests_simplify :: TestTree
tests_simplify = testGroup "simplify" 
    [ 
        testCase "Atom" $ simplify p1 @?= p1,
        testCase "NOT Atom" $ simplify (Not p1) @?= Not p1,
        testCase "OR" $ simplify  simpleOr @?= simpleOr,
        testCase "AND" $ simplify simpleAnd @?= simpleAnd,
        testCase "=>" $ simplify simpleHence @?= (Not p1) `Or` p2,
        testCase "<=>" $ simplify simpleEq @?= ((Not p1) `Or` p2) `And`((Not p2) `Or` p1),

        testCase "complex =>" $ simplify (p1 :-> p2 :-> p3) @?= (Not p1) `Or` (Not p2) `Or` p3,
        testCase "EQ + =>" $ simplify ((p1 :<->: p2) :-> p3) @?= ((Not (( (Not p1) `Or` p2 ) `And` ((Not p2) `Or` p1))) `Or` p3),
        testCase "AND + EQ" $ simplify ((p1 `And` p2) :<->: p1) @?= ((Not (p1 `And` p2)) `Or` p1) `And` ((Not p1) `Or` (p1 `And` p2))
    ]

tests_NNF :: TestTree
tests_NNF = testGroup "NNF"
    [
        testCase "Atom" $ toNNF p1 @?= p1,
        testCase "OR" $ toNNF simpleOr @?= simpleOr,
        testCase "AND" $ toNNF simpleAnd @?= simpleAnd,
        testCase "Double NOT" $ toNNF (Not (Not p1)) @?= p1,
        testCase "Not Atom" $ toNNF (Not p1) @?= (Not p1),
        testCase "=>" $ toNNF simpleHence @?= ((Not p1) `Or` p2),
        testCase "<=>" $ toNNF simpleEq @?= ((Not p1) `Or` p2) `And`((Not p2) `Or` p1),
        testCase "De Morgan's law1" $ toNNF (Not (p1 `Or` p2)) @?= ((Not p1) `And` (Not p2)),
        testCase "De Morgan's law2" $ toNNF (Not (p1 `And` p2)) @?= ((Not p1) `Or` (Not p2)),

        testCase "De Morgan's + (NOT) inside OR" $ toNNF (Not ((Not p1) `Or` (Not p2))) @?= p1 `And` p2,
        testCase "De Morgan's + AND" $ toNNF (Not (p1 `And` (Not p2))) @?= (Not p1) `Or` p2,
        testCase "Three agrs OR" $ toNNF (Not (p1 `Or` p2 `Or` p3)) @?= (Not p1) `And` (Not p2) `And` (Not p3),
        testCase "Double NOT + two brackets" $ toNNF ((Not (p1 `And` p2)) `Or` (Not (Not p3))) @?= ((Not p1) `Or` (Not p2)) `Or` p3,
        testCase "So much NOT's even" $ toNNF (Not (Not (Not (Not p1)))) @?= p1, 
        testCase "So much NOT's odd" $ toNNF (Not (Not (Not (Not (Not p1))))) @?= (Not p1)
    ]

tests_DNF :: TestTree
tests_DNF = testGroup "DNF"
    [
        testCase "Atom" $ toDNF p1 @?= p1,
        testCase "Not Atom" $ toDNF (Not p1) @?= (Not p1),
        testCase "OR" $ toDNF simpleOr @?= simpleOr, 
        testCase "AND" $ toDNF simpleAnd @?= simpleAnd,
        testCase "=>" $ toDNF simpleHence @?= ((Not p1) `Or` p2),
        testCase "<=>" $ toDNF simpleEq @?= (((Not p2) `And` (Not p1)) `Or` (p1 `And` (Not p1))) `Or` (((Not p2) `And` p2) `Or` (p1 `And` p2)),

        testCase "AND right distributivity" $ toDNF ((p1 `Or` p2) `And` p3) @?= ((p1 `And` p3) `Or` (p2 `And` p3)),
        testCase "AND left distributivity" $ toDNF (p1 `And` (p2 `Or` p3)) @?= ((p2 `And` p1) `Or` (p3 `And` p1))
    ]

tests_CNF :: TestTree
tests_CNF = testGroup "CNF"
    [
        testCase "Atom" $ toCNF p1 @?= p1,
        testCase "Not Atom" $ toCNF (Not p1) @?= (Not p1),
        testCase "OR" $ toCNF simpleOr @?= simpleOr,
        testCase "AND" $ toCNF simpleAnd @?= simpleAnd,
        testCase "=>" $ toCNF simpleHence @?= ((Not p1) `Or` p2),
        testCase "<=>" $ toCNF simpleEq @?= ((Not p1) `Or` p2) `And` ((Not p2) `Or` p1),

        testCase "OR right distributivity" $ toCNF ((p1 `And` p2) `Or` p3) @?= (p1 `Or` p3) `And` (p2 `Or` p3),
        testCase "OR left distributivity" $ toCNF (p3 `Or` (p1 `And` p2)) @?= (p1 `Or` p3) `And` (p2 `Or` p3)
    ]

tests_Parser :: TestTree
tests_Parser = testGroup "Parser"
    [
        testCase "Atom" $ (read ("alik") :: Formula) @?= (Atom "alik"),
        testCase "Double NOT" $ (read ("! ! p") :: Formula) @?= (Not (Not (Atom "p"))),
        testCase "AND priority" $ (read ("p1 && p2 || p3") :: Formula) @?= p1 `And` p2 `Or` p3,
        testCase "Hence associativity" $ (read ("p1 -> p2 -> p3") :: Formula) @?= p1 :-> p2 :-> p3
    ]