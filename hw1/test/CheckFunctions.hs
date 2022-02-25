module CheckFunctions where
import Formula 

import DNF
import CNF

checkNNF :: Formula -> Bool
checkNNF f = case f of 
                    Atom s -> True
                    Not (Atom s) -> True 
                    (a `And` b) -> (checkNNF a) && (checkNNF b)
                    (a `Or` b) -> (checkNNF a) && (checkNNF b)
                    otherwise -> False 

checkCNFClause :: Formula -> Bool
checkCNFClause f = case f of 
                         Atom s -> True 
                         Not (Atom _) -> True
                         (a `Or` b) -> (checkCNFClause a) && (checkCNFClause b)
                         otherwise -> False

checkCNF :: Formula -> Bool
checkCNF f = case f of 
                    (a `And` b) -> (checkCNF a) && (checkCNF b)
                    otherwise -> checkCNFClause f  

checkDNFClause :: Formula -> Bool
checkDNFClause f = case f of 
                         Atom _ -> True
                         Not (Atom _) -> True
                         (a `And` b) -> (checkDNFClause a) && (checkDNFClause b)
                         otherwise -> False

checkDNF :: Formula -> Bool
checkDNF f = case f of 
                    (a `Or` b) -> (checkDNF a) && (checkDNF b)
                    otherwise -> checkDNFClause f

