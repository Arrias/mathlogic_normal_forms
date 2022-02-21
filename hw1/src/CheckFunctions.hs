module CheckFunctions where
import Formula

import DNF
import CNF

checkNNF :: Formula -> Bool
checkNNF (Atom s) = True
checkNNF (Not (Atom s)) = True
checkNNF (a `And` b) = (checkNNF a) && (checkNNF b)
checkNNF (a `Or` b) = (checkNNF a) && (checkNNF b)
checkNNF _ = False 

checkCNFClause :: Formula -> Bool
checkCNFClause (Atom _) = True
checkCNFClause (Not (Atom _)) = True
checkCNFClause (a `Or` b) = (checkCNFClause a) && (checkCNFClause b)
checkCNFClause _ = False

checkCNF :: Formula -> Bool
checkCNF (a `And` b) = (checkCNF a) && (checkCNF b)
checkCNF a = checkCNFClause a 

checkDNFClause :: Formula -> Bool
checkDNFClause (Atom _) = True
checkDNFClause (Not (Atom _)) = True 
checkDNFClause (a `And` b) = (checkDNFClause a) && (checkDNFClause b)
checkDNFClause _ = False

checkDNF :: Formula -> Bool
checkDNF (a `Or` b) = (checkDNF a) && (checkDNF b)
checkDNF a = checkDNFClause a