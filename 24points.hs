import Data.List (permutations)
import System.Environment (getArgs)
import Control.Applicative (liftA2)

-- utility functions
parseInt :: String -> Int
parseInt x = read x::Int

combinations :: [a] -> Int -> [[a]]
combinations arr n = mapM (const arr) [1..n]

-- double level Maybe to single level
foldMaybe :: Maybe (Maybe a) -> Maybe a
foldMaybe Nothing = Nothing
foldMaybe (Just x) = x

safediv :: Int -> Int -> Maybe Int
safediv _  0 = Nothing    -- division by zero
safediv a b =
  case (a `divMod` b) of
    (x, 0) -> Just x
    _ -> Nothing

findFirst :: (a -> Maybe b) -> [a] -> Maybe b
findFirst _ [] = Nothing
findFirst condition (h:tail) =
  case (condition h) of
    Just x -> Just x
    _ -> findFirst condition tail

-- data structures
data BinOp = Add | Sub | Mul | Div

data FormulaTree = Num Int |
                   Arith FormulaTree BinOp FormulaTree

showChildMul :: FormulaTree -> String
showChildMul child =
  case child of
    (Arith left Add right) -> "(" <> (show child) <> ")"
    (Arith left Sub right) -> "(" <> (show child) <> ")"
    (Arith left Div right) -> "(" <> (show child) <> ")"
    _ -> (show child)

showChildDiv :: FormulaTree -> String
showChildDiv child =
  case child of
    (Arith left op right) -> "(" <> (show child) <> ")"
    _ -> (show child)

showChildAddSub :: FormulaTree -> String
showChildAddSub child =
  case child of
    (Arith left Add right) -> "(" <> (show child) <> ")"
    (Arith left Sub right) -> "(" <> (show child) <> ")"
    _ -> (show child)

instance Show FormulaTree where
  show (Num num) = show num
  show (Arith left Add right) = (showChildAddSub left) <> " + " <> (showChildAddSub right)
  show (Arith left Sub right) = (showChildAddSub left) <> " - " <> (showChildAddSub right)
  show (Arith left Mul right) = (showChildMul left) <> " * " <> (showChildMul right)
  show (Arith left Div right) = (showChildDiv left) <> " / " <> (showChildDiv right)

-- evaluate the formula tree
evaluateTree :: FormulaTree -> Maybe Int
evaluateTree (Num num) = Just num
evaluateTree (Arith left Add right) = liftA2 (+) (evaluateTree left) (evaluateTree right)
evaluateTree (Arith left Sub right) = liftA2 (-) (evaluateTree left) (evaluateTree right)
evaluateTree (Arith left Mul right) = liftA2 (*) (evaluateTree left) (evaluateTree right)
evaluateTree (Arith left Div right) = foldMaybe $ liftA2 safediv (evaluateTree left) (evaluateTree right)

calcTree :: FormulaTree -> Maybe FormulaTree
calcTree tree =
  case (evaluateTree tree) of
    Just 24 -> Just tree    -- Got the right solution
    _ -> Nothing

possibleTrees :: [Int] -> [BinOp] -> [FormulaTree]
possibleTrees numbers ops =
  -- there are 3 kinds of trees
  let
    a = Num (numbers!!0)
    b = Num (numbers!!1)
    c = Num (numbers!!2)
    d = Num (numbers!!3)
    op1 = ops!!0
    op2 = ops!!1
    op3 = ops!!2
  in
    [
      Arith (Arith (Arith a op1 b) op2 c) op3 d,
      Arith (Arith a op1 b) op2 (Arith c op3 d),
      Arith a op1 (Arith b op2 (Arith c op3 d))
    ]

calcNumbers :: [Int] -> Maybe FormulaTree
calcNumbers numbers =
  findFirst (\nums ->
               findFirst
               (calcNumbersAndOps nums)
               (combinations [Add, Sub, Mul, Div] 3))
  (permutations numbers)

calcNumbersAndOps :: [Int] -> [BinOp] -> Maybe FormulaTree
calcNumbersAndOps nums ops =
  findFirst calcTree (possibleTrees nums ops)

-- Usage: ./24points a b c d
-- where a b c d are numbers [1-9]
main = do
  args <- getArgs
  -- TODO: senity check against command line arguments
  let numbers = map parseInt args
  let res = calcNumbers numbers
  print(res)
