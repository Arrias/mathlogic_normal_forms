module Formula where

import Text.Read 
import Data.List

data Formula = Atom { _name :: String } 
			 | Not Formula  
			 | Formula `Or` Formula   
			 | Formula `And` Formula 
			 | Formula :-> Formula 
			 | Formula :<->: Formula
			 	deriving Eq

infixr 8 `Not`
infixr 7 `And`
infixr 7 :->
infixr 7 :<->:
infixr 6 `Or`

instance Show Formula where
	showsPrec p f = case f of 
		(Atom s) -> showString s
	  	(Not f) -> showParen True $ showString "!" . showsPrec 10 f
		(x `Or` y) -> showParen True $ (showsPrec 10 x) . showString " || " . (showsPrec 10 y)
		(x `And` y) -> showParen True $ (showsPrec 10 x) . showString " && " . (showsPrec 10 y)
		(x :-> y) -> showParen True $ (showsPrec 10 x) . showString " -> " . (showsPrec 10 y)
		(x :<->: y) -> showParen True $ (showsPrec 10 x) . showString " <-> " . (showsPrec 10 y)

type Expr = [String]

-- split string into words
getWords :: String -> Expr
getWords ar = case ar of 
				[] -> []
				otherwise -> let res = head $ lex ar in (fst res) : (getWords $ snd res)

getPriority :: String -> Maybe Int
getPriority s | (s == "||") = (Just 6)
			  | (s == "&&" || s == "->" || s == "<->") = (Just 7)
			  | (s == "!") = (Just 8)
			  | otherwise = Nothing

-- returns all operations of expr in format (bracket nesting, priority, id)
getOpersOfExpr :: Expr -> [(Int, Int, Int)]
getOpersOfExpr expr = getOpersHelper 0 0 expr 
						where getOpersHelper bal id expr = case expr of 
								[] -> []
								("(":xs) -> getOpersHelper (succ bal) (succ id) xs 
								(")":xs) -> getOpersHelper (pred bal) (succ id) xs 
								(x:xs) -> case (getPriority x) of 
									(Just p) -> (bal, p, id) : (getOpersHelper bal (succ id) xs)
									otherwise -> getOpersHelper bal (succ id) xs 
				    
-- returns id of last operation of expr
-- retunrs Nothing if expr contains extra parentheses around the edges, like "((p1))" 
findLastOperation :: Expr -> Maybe Int  
findLastOperation ar = case (sort $ getOpersOfExpr ar) of 
							[] -> Nothing
							((x,y,z):xs) -> if (x > 0) then Nothing
											else Just z	
-- formula constructor by name
fmlConsByStr :: String -> (Formula -> Formula -> Formula)
fmlConsByStr s = case s of 
						"||" -> Or 
						"&&" -> And 
						"->" -> (:->)
						"<->" -> (:<->:)

-- formula parser
hprFmlRdr :: Expr -> Formula
hprFmlRdr expr = case expr of 
					[name] -> Atom name 
					otherwise -> case (findLastOperation expr) of 
						Nothing -> hprFmlRdr (tail . init $ expr)
						(Just pos) -> let oper = (expr !! pos) in 
							if (oper == "!") then Not $ hprFmlRdr (drop (succ pos) expr)
							else (fmlConsByStr oper) (hprFmlRdr (take pos expr)) (hprFmlRdr (drop (succ pos) expr))
								  		
instance Read Formula where 
	readsPrec _ str = [(hprFmlRdr . getWords $ str, "")]