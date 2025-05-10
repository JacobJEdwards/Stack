module Main (main) where

class Assignment where
    empty :: Assignment
    assign :: Assignment -> Literal -> Bool -> Assignment
    isAssigned :: Assignment -> Literal -> Maybe Bool
    isComplete :: Assignment -> Bool
    getUnassigned :: Assignment -> [Literal]

data Solver = Solver {
    clauses :: [Clause],
    assignment :: Assignment,
}


main :: IO ()
main = undefined
