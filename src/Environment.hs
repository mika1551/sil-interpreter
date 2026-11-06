module Environment
  ( Environment
  , ScopeKind (..)
  , empty
  , pushScope
  , popScope
  , define
  , assign
  , lookup
  ) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map

data ScopeKind = BindingScope | VariableScope
  deriving (Eq, Show)

data Environment a
  = Global (Map.Map String a)
  | Local ScopeKind (Map.Map String a) (Environment a)
  deriving (Eq, Show)

empty :: Environment a
empty = Global Map.empty

pushScope :: ScopeKind -> Environment a -> Environment a
pushScope kind = Local kind Map.empty

popScope :: Environment a -> Maybe (Environment a)
popScope (Global _) = Nothing
popScope (Local _ _ outer) = Just outer

define :: String -> a -> Environment a -> Environment a
define name value (Global bindings) = Global (Map.insert name value bindings)
define name value (Local kind bindings outer) =
  Local kind (Map.insert name value bindings) outer

lookup :: String -> Environment a -> Maybe a
lookup name (Global bindings) = Map.lookup name bindings
lookup name (Local _ bindings outer) = case Map.lookup name bindings of
  Just value -> Just value
  Nothing -> lookup name outer

assign :: String -> a -> Environment a -> Environment a
assign name value environment = case update environment of
  Just updated -> updated
  Nothing -> introduce environment
  where
    update (Global bindings)
      | Map.member name bindings = Just (Global (Map.insert name value bindings))
      | otherwise = Nothing
    update (Local kind bindings outer)
      | Map.member name bindings = Just (Local kind (Map.insert name value bindings) outer)
      | otherwise = Local kind bindings <$> update outer
    introduce (Local BindingScope bindings outer) =
      Local BindingScope bindings (introduce outer)
    introduce owner = define name value owner
