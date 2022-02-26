module Main where
import System.Environment
import Formula
import DNF
import CNF
import NNF 

main :: IO ()
main = do 
    args <- getArgs 
    let frm = (read (args !! 0)) :: Formula
    putStrLn $ "NNF: " ++ (show $ toNNF frm)
    putStrLn $ "DNF: " ++ (show $ toDNF frm)
    putStrLn $ "CNF: " ++ (show $ toCNF frm)