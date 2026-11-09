module PatternMatrix
  ( Decision (..), Head (..), Occurrence, compileMatrix, patternBindings ) where

import AST
import Data.List (findIndex)
import Data.Maybe (mapMaybe)

type Occurrence = [Int]

data Head
  = NumberHead Int
  | StringHead String
  | ArrayHead Int
  | ConstructorHead String Int
  | TypeHead PatternType
  deriving (Eq, Show)

data Decision
  = Failure
  | Success Int
  | Test Occurrence Head Decision Decision
  deriving (Eq, Show)

compileMatrix :: [Pattern] -> Decision
compileMatrix patterns = matrix [[]] [([strip p], branch) | (branch, p) <- zip [0..] patterns]

matrix :: [Occurrence] -> [([Pattern], Int)] -> Decision
matrix _ [] = Failure
matrix columns rows@((first, branch) : _) = case findIndex (/= Wildcard) first of
  Nothing -> Success branch
  Just column ->
    let chosen = headOf (first !! column)
        occurrence = columns !! column
        children = case chosen of
          TypeHead _ -> [occurrence]
          _ -> [occurrence ++ [i] | i <- [0 .. arity chosen - 1]]
        replace xs ys = take column xs ++ ys ++ drop (column + 1) xs
        yes (patterns, action) = do
          specialized <- specialize chosen (patterns !! column)
          pure (replace patterns specialized, action)
        no (patterns, _) = not (covered chosen (patterns !! column))
    in Test occurrence chosen
         (matrix (replace columns children) (mapMaybe yes rows))
         (matrix columns (filter no rows))

strip :: Pattern -> Pattern
strip (BindPattern _ nested) = strip nested
strip p = p

headOf :: Pattern -> Head
headOf p = case p of
  NumberPattern n -> NumberHead n
  StringPattern s -> StringHead s
  ArrayPattern ps -> ArrayHead (length ps)
  SExpressionPattern name ps -> ConstructorHead name (length ps)
  TypePattern kind -> TypeHead kind
  _ -> error "unconstrained matrix column"

arity :: Head -> Int
arity (ArrayHead n) = n
arity (ConstructorHead _ n) = n
arity _ = 0

headType :: Head -> PatternType
headType h = case h of
  NumberHead _ -> IntType
  StringHead _ -> StringType
  ArrayHead _ -> ArrayType
  ConstructorHead _ _ -> SExpressionType
  TypeHead kind -> kind

specialize :: Head -> Pattern -> Maybe [Pattern]
specialize h original = case strip original of
  Wildcard -> Just unconstrained
  TypePattern kind | kind == headType h -> Just unconstrained
  p | TypeHead kind <- h, headType (headOf p) == kind -> Just [p]
  p | headOf p == h -> Just $ case p of
    ArrayPattern ps -> map strip ps
    SExpressionPattern _ ps -> map strip ps
    _ -> []
  _ -> Nothing
  where
    unconstrained = replicate (case h of TypeHead _ -> 1; _ -> arity h) Wildcard

covered :: Head -> Pattern -> Bool
covered h original = case strip original of
  Wildcard -> False
  p | TypeHead kind <- h -> headType (headOf p) == kind
  p -> headOf p == h

patternBindings :: Pattern -> [(String, Occurrence)]
patternBindings = walk []
  where
    walk path p = case p of
      BindPattern name nested -> walk path nested ++ [(name, path)]
      ArrayPattern ps -> children path ps
      SExpressionPattern _ ps -> children path ps
      _ -> []
    children path ps = concat [walk (path ++ [i]) p | (i, p) <- zip [0..] ps]
