{-# LANGUAGE InstanceSigs #-}

import Formula
import CheckFunctions
import DNF
import CNF 
import NNF
import Test.QuickCheck
import Test.QuickCheck.All

vars_names = fmap (\x -> show x) ['a'..'f'] 
ln = pred $ length vars_names 

formula_variants f1 f2 = [(3, f1 `And` f2),
                          (3, f1 `Or` f2),
                          (1, f1 :-> f2),
                          (1, f1 :<->: f2),
                          (3, Not f1),
                          (3, f2 `And` f1),
                          (3, f2 `Or` f1),
                          (1, f2 :-> f1),
                          (1, f2 :<->: f1)]

instance (Arbitrary Formula) where
    arbitrary :: Gen Formula
    arbitrary = sized heightedArbitrary 
        where heightedArbitrary 0 = do 
                                    id <- choose (0, ln)
                                    return $ Atom (vars_names !! id)
              heightedArbitrary n = do 
                                    small <- choose(0, n - 1)
                                    f1 <- (resize (pred n) arbitrary)
                                    f2 <- (resize small arbitrary)
                                    frequency $ fmap (\(x, y) -> (x, return y)) (formula_variants f1 f2)

checkNNFAlgo x = checkNNF . toNNF $ x
    where types = x :: Formula
checkCNFAlgo x = checkCNF . toCNF $ x
    where types = x :: Formula
checkDNFAlgo x = checkDNF . toDNF $ x
    where types = x :: Formula

checkAllForms formulaSize = 
                    mapM_ (quickCheck . (mapSize $ const formulaSize)) 
                                    [checkNNFAlgo, checkCNFAlgo, checkDNFAlgo]

main :: IO ()
main = do
    let formula_height_for_tests = 5
    putStrLn $ "formula height used: " ++ (show formula_height_for_tests)
    checkAllForms formula_height_for_tests