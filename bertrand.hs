import Data.Maybe

data Operator = Operator { name :: String, commute :: Bool, etor :: Int -> Int -> Int, vtor :: Int -> Int -> Bool }
instance Show Operator where show o = name o

data Operation = Operation Operator (Int,Int)
instance Show Operation where show (Operation op (op1, op2)) = show op1 ++ name op ++ show op2 ++ "=" ++ show ((etor op) op1 op2)  

allOprtrs :: [Operator]
allOprtrs = [addOp,mulOp,subOp,divOp] where 
    addOp=Operator{name="+", commute=True,  etor=(\x y -> x+y),       vtor=(\_ _ -> True)}
    mulOp=Operator{name="*", commute=True,  etor=(\x y -> x*y),       vtor=(\x y -> x /= 1 && y /= 1 && x /= 0 && y /= 0)}
    subOp=Operator{name="-", commute=False, etor=(\x y -> x-y),       vtor=(\x y -> x > y)}
    divOp=Operator{name="/", commute=False, etor=(\x y -> x `div` y), vtor=(\x y -> y /= 0 && y /=1 && (x `mod` y) == 0)}

valuesDerivation :: [Int] -> (Int,Int) -> Operator -> Maybe([Int], Operation)
valuesDerivation values (idx1, idx2) op
    | values == []               = Nothing
    | (commute op) && i > j      = Nothing
    | (vtor op) i j == False     = Nothing
    | otherwise                  = Just ((etor op) i j:[val | (val, idx) <- zip values [0..], idx /= idx1 && idx /= idx2], Operation op (i,j))
    where i=(values!!idx1) 
          j=(values!!idx2)

allValuesDerivations :: [Operator] -> [Int] -> [([Int], Operation)]
allValuesDerivations oprtrs vals = (indexPairs vals) >>= (\pair -> valuesDerivations oprtrs vals pair)
    where 
        indexPairs :: [a] -> [(Int,Int)]
        indexPairs xs = [(i,j) | i <- [0..length vals - 1], j <- [0..length vals - 1], i /= j]
        valuesDerivations :: [Operator] -> [Int] -> (Int, Int) -> [([Int], Operation)]
        valuesDerivations oprtrs vals indexes = catMaybes (map (\op -> valuesDerivation vals indexes op) oprtrs)

allProblemAndOprtrsDerivations :: [Operator] -> [Int] -> [Operation] -> [([Int],[Operation])]
allProblemAndOprtrsDerivations oprtrs vals oprtns =
    map (\(derivedValues, derivedOp) -> (derivedValues, oprtns ++ [derivedOp])) (allValuesDerivations oprtrs vals) 

internalSolve ::[Operator] -> [Int] -> Int -> [Operation] -> Maybe [Operation]
internalSolve oprtrs vals target oprtns
    | elem target vals = Just oprtns
    | otherwise        = case filter isJust derivedSolutions of
                                []    -> Nothing
                                (x:_) -> x
    where derivations = allProblemAndOprtrsDerivations oprtrs vals oprtns
          derivedSolutions = map (\(derivedValues, derivedOprtns) -> internalSolve oprtrs derivedValues target derivedOprtns) derivations

solve :: [Operator] -> [Int] -> Int -> Maybe [Operation]
solve oprtrs vals target = internalSolve oprtrs vals target []


