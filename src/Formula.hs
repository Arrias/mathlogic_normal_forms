module Formula where

import Text.Read 

data Formula = Atom { _name :: String } 
			 | Not Formula  
			 | Formula `Or` Formula   
			 | Formula `And` Formula 
			 | Formula :-> Formula 
			 | Formula :<->: Formula
			 	deriving Eq

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

-- split string into words
getWords ar = case ar of 
				[] -> []
				otherwise -> let res = head $ lex ar in (fst res) : (getWords $ snd res)

-- find matching bracket for first open bracket 
prefPsp 1   (")" : xs) = ([], xs)
prefPsp bal ("(" : xs) = ("(" : (fst res), snd res)
            where res = prefPsp (bal + 1) xs
prefPsp bal (")" : xs) = (")" : (fst res), snd res)
            where res = prefPsp (bal - 1) xs
prefPsp bal (x   : xs) = ( x  : (fst res), snd res)
            where res = prefPsp bal xs

-- split string into two parts [first psp, other]
getPrefPsp (x : xs) = prefPsp 1 xs 

sptFstBrck ("(":xs) = ([], "(":xs)
sptFstBrck (a:[]) = ([], [a])
sptFstBrck (x:xs) = (x : (fst res), snd res)
            where res = sptFstBrck xs

-- formula constructor by name
fmlConsByStr s = case s of 
						"||" -> Or 
						"&&" -> And 
						"->" -> (:->)
						"<->" -> (:<->:)

-- helper formula reader
hprFmlRdr mas = case mas of 
						[name] -> Atom name 
						("!" : xs) -> Not $ hprFmlRdr xs  
						("(" : xs) -> if (length sp == 0) then hprFmlRdr fp  
							          else let cons = fmlConsByStr $ head sp in cons (hprFmlRdr fp) (hprFmlRdr $ tail sp)
								where fp = fst $ getPrefPsp mas;
									  sp = snd $ getPrefPsp mas; 	
						otherwise -> let cons = fmlConsByStr $ last fp in cons (hprFmlRdr (init fp)) (hprFmlRdr sp)
							where fp = fst $ sptFstBrck mas;
								  sp = snd $ sptFstBrck mas;
									       
readFormula :: String -> Formula
readFormula = hprFmlRdr . getWords 
					  	
instance Read Formula where 
	readsPrec _ str = [(readFormula str, "")]