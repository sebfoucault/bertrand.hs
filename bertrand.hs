import Data.Maybe

data Operator = Operator { name :: String, commute :: Bool, etor :: Int -> Int -> Int, vtor :: Int -> Int -> Bool }
instance Show Operator where show o = name o

data Operation = Operation Operator (Int,Int)
instance Show Operation where show (Operation op (op1, op2)) = show op1 ++ name op ++ show op2 ++ "=" ++ show ((etor op) op1 op2)  

data ProblemAndOps = ProblemAndOps { values :: [Int], target :: Int, operations :: [Operation] } deriving (Show)

allOps :: [Operator]
allOps = [addOp,mulOp,subOp,divOp] where 
    addOp=Operator{name="+", commute=True,  etor=(\x y -> x+y),       vtor=(\_ _ -> True)}
    mulOp=Operator{name="*", commute=True,  etor=(\x y -> x*y),       vtor=(\x y -> x /= 1 && y /= 1 && x /= 0 && y /= 0)}
    subOp=Operator{name="-", commute=False, etor=(\x y -> x-y),       vtor=(\x y -> x > y)}
    divOp=Operator{name="/", commute=False, etor=(\x y -> x `div` y), vtor=(\x y -> y /= 0 && y /=1 && (x `mod` y) == 0)}

valuesDerivation :: [Int] -> (Int,Int) -> Operator -> Maybe([Int], Operation)
valuesDerivation xs (idx1, idx2) op
    | xs == []               = Nothing
    | (commute op) && i > j  = Nothing
    | (vtor op) i j == False = Nothing
    | otherwise              = Just ([fst e | e <- zip xs [0..], snd e /= idx1 && snd e /= idx2] ++ [(etor op) i j], Operation op (i,j))
    where i=(xs!!idx1) 
          j=(xs!!idx2)

valuesDerivations :: [Operator] -> [Int] -> (Int, Int) -> [([Int], Operation)]
valuesDerivations ops xs indexes = catMaybes(map (\op -> valuesDerivation xs indexes op) ops)

allValuesDerivations :: [Operator] -> [Int] -> [([Int], Operation)]
allValuesDerivations ops xs = (indexPairs xs) >>= (\pair -> valuesDerivations ops xs pair)
    where indexPairs :: [a] -> [(Int,Int)]
          indexPairs xs = [(i,j) | i <- [0..length xs - 1], j <- [0..length xs - 1], i /= j]

allProblemAndOpsDerivations :: [Operator] -> ProblemAndOps -> [ProblemAndOps]
allProblemAndOpsDerivations ops pnops =
        map (\derivation -> deriveProblemAndOp pnops derivation) (allValuesDerivations ops (values pnops)) 
    where 
        pnopOps = operations pnops
        deriveProblemAndOp :: ProblemAndOps -> ([Int], Operation) -> ProblemAndOps
        deriveProblemAndOp pnops (derivedValues, derivedOp) = ProblemAndOps{values=derivedValues, target=target pnops, operations=pnopOps ++ [derivedOp]}

internalSolve ::[Operator] -> ProblemAndOps -> Maybe [Operation]
internalSolve ops pnops
    | elem (target pnops) (values pnops)      = Just(operations pnops)
    | otherwise                               = case derivedSolutions of
                                                    []        -> Nothing
                                                    (x:xs)    -> x
    where 
        derivations = allProblemAndOpsDerivations ops pnops
        derivedSolutions = filter isJust (map (\derivation -> internalSolve ops derivation) (derivations))

solve :: [Operator] -> [Int] -> Int -> Maybe [Operation]
solve ops values target = internalSolve ops ProblemAndOps{values=values, target=target, operations=[]}
