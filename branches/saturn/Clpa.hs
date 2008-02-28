{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Clpa where

import Data.Maybe
import Data.List
import Data.Char
import Text.PrettyPrint

import Control.Monad
import Util
import DataStructures
import MakeTeaMonad

clpaDefinition :: MakeTeaMonad String 
clpaDefinition = do
	conjForwardDecls <- withConj $ mapM createConjForwardDecls
	disjForwardDecls <- withDisj $ mapM createDisjForwardDecls
	conjTypes <- withConj $ mapM createConjTypes
	disjTypes <- withDisj $ mapM createDisjTypes
	predicates <- withConj $ mapM convertToPredicate
	return $ (unlines 
		[
		  "% Forward declarations for conjunctive types"
		, unlines conjForwardDecls
		, ""
		, "% Forward declarations for disjunctive types"
		, unlines disjForwardDecls
		, ""
		, "% Disjunctive types"
 	 	, unlines disjTypes
		, ""
		, "% Conjunctive types"
		, unlines conjTypes
		, ""
		, "% Predicates"
		, unlines predicates
		])

convertToPredicate :: Rule Conj -> MakeTeaMonad String
convertToPredicate (Conj head body) = do
	predName <- toPredName head
	args <- forM body $ \term -> do
		argType <- elim termToParam term
		let argName = toVarName term
		return (argName ++ ":" ++ argType) 
	return $ "predicate " ++ predName ++ " (" ++ flattenComma (args) ++ ")."

createConjTypes :: Rule Conj -> MakeTeaMonad String
createConjTypes (Conj head body) = do
	typeName <- toTypeName head
	args <- forM body $ \term -> do
		argType <- elim (termToParam) term
		return argType
	return $ "type " ++ typeName ++ " ::= " ++ typeName ++ " {" ++ (flattenComma args) ++ "}."

createConjForwardDecls :: Rule Conj -> MakeTeaMonad String
createConjForwardDecls (Conj head body) = do
	typeName <- toTypeName head
	return $ "type " ++ typeName ++ "."

createDisjForwardDecls :: Rule Disj -> MakeTeaMonad String
createDisjForwardDecls (Disj head _) = do
	typeName <- toTypeName head
	return $ "type " ++ typeName ++ "."


createDisjTypes :: Rule Disj -> MakeTeaMonad String
createDisjTypes (Disj head _) = do
	typeName <- toTypeName head
	inst <- concreteInstances head -- TODO should this be allInstances?
	body <- forM inst $ \term -> do
		tn <- toTypeName term
		return tn
--	if typeName == "php_node" then return ""
--		else 
	return $ "type " ++ typeName ++ " ::= \n\t\t  " ++ (flattenPipe (map (++ "\n\t\t" ) body)) ++ "."

termToParam :: Term a -> MakeTeaMonad String
termToParam t@(Term lab sym mult) = do
	tp <- toTypeName (Term lab sym Single)
	return $ if isVector mult 
		then "list[" ++ tp ++ "]"
		else tp
termToParam m@(Marker _ _) = return "bool" 




{- Turn types into Predicate names -}

class ToPredName a where
	toPredName :: a -> MakeTeaMonad (Name Class) -- TODO Class?

instance ToPredName (Symbol a) where
	toPredName = symbolToPredName

instance ToPredName (Some Symbol) where
	toPredName = elim symbolToPredName

instance ToPredName (Term NonMarker) where
	toPredName = termToPredName

symbolToPredName :: Symbol a -> MakeTeaMonad (Name Class) -- TODO Class?
symbolToPredName (NonTerminal n) = return ("phc_" ++ strToLower n) 
symbolToPredName (Terminal n _) = return ("phc_token_" ++ strToLower n) 

termToPredName :: Term NonMarker -> MakeTeaMonad CType 
termToPredName (Term _ s m) = do
	cn <- elim symbolToPredName s
	if isVector m 
		then do
			return ("list of " ++ cn)
		else return cn


{- Turn types into variable names -}
class ToVarName a where
	toVarName :: a -> Name Variable 

instance ToVarName (Term a) where
	toVarName = termToVarName

instance ToVarName (Some Term) where
	toVarName = elim termToVarName

instance ToVarName (Symbol a) where
	toVarName = symbolToVarName

instance ToVarName (Some Symbol) where
	toVarName = elim symbolToVarName

termToVarName :: Term a -> Name Variable 
termToVarName (Term Nothing s m) 
	| isVector m = toVarName s ++ "S" 
	| otherwise = toVarName s
termToVarName (Term (Just n) _ _) = n
termToVarName (Marker Nothing m) = "IS_" ++ (map toUpper m)
termToVarName (Marker (Just n) _) = n

symbolToVarName :: Symbol a -> Name Variable 
symbolToVarName (NonTerminal n) = map toUpper n
symbolToVarName (Terminal n _) = map toUpper n



{- Turn types into type names -}
class ToTypeName a where
	toTypeName :: a -> MakeTeaMonad (Name Class)

instance ToTypeName (Symbol a) where
	toTypeName = symbolToTypeName

instance ToTypeName (Some Symbol) where
	toTypeName = elim symbolToTypeName

instance ToTypeName (Term NonMarker) where
	toTypeName = termToTypeName

symbolToTypeName :: Symbol a -> MakeTeaMonad (Name Class)
symbolToTypeName (NonTerminal n) = return ("php_" ++ strToLower n)
symbolToTypeName (Terminal n _) = return ("php_token_" ++ strToLower n) 

termToTypeName :: Term NonMarker -> MakeTeaMonad CType 
termToTypeName (Term _ s m) = do
	cn <- elim symbolToTypeName s
	if isVector m 
		then do
			return ("list [" ++ cn ++ "]")
		else return cn
