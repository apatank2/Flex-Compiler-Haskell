replic [] = []
replic (x:xs) = if x < 0 then error "negative value"
	            else if check(xs) == True then error "negative value"
	            else map (copy x) [x] ++ replic xs

copy y xs = if y == 0 then [] 
		  else if y < 0 then error "negative value"
          else xs : copy (y-1) xs
				   
check [] = False
check (x:xs) = if x < 0 then True
               else check xs

data Formula = Atom Bool
	|And Formula Formula
	|Or Formula Formula
	|Implies Formula Formula
	|Not Formula

eval :: Formula -> Bool
eval (Atom x) = x
eval (And f1 f2) = (eval f1) && (eval f2)
eval (Or f1 f2) = (eval f1) || (eval f2)
eval (Implies f1 f2) = (eval f1) <= (eval f2)  
eval (Not f1) = not (eval f1)

collect_atoms :: Formula -> [Bool]
collect_atoms (Atom y) =  [y]
collect_atoms (And f1 f2) =  (collect_atoms f1) ++ (collect_atoms f2)
collect_atoms (Or f1 f2) =  (collect_atoms f1)  ++ (collect_atoms f2)
collect_atoms (Implies f1 f2) =  (collect_atoms f1) ++ (collect_atoms f2)
collect_atoms (Not f1) = (collect_atoms f1)