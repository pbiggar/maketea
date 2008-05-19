{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

-- TODO:
--		anything nullable (is_option?) should be Maybe

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
	tokenDecls <- withTokens $ mapM createTokenDecls
	conjTypes <- withConj $ mapM createConjTypes
	disjTypes <- withDisj $ mapM createDisjTypes
	conjPreds <- withConj $ mapM createConjPreds
	tokenPreds <- withTokens $ mapM createTokenPreds
	return $ (unlines 
		[ "session ast (PROG:string)."
		, ""
		, "% Type not supplied by Saturn"
		, "type null."
		, "type id = int."
		, ""
		, "% Forward declarations for conjunctive types"
		, unlines conjForwardDecls
		, ""
		, "% Forward declarations for disjunctive types"
		, unlines disjForwardDecls
		, ""
		, "% Token declarations"
		, unlines tokenDecls
		, ""
		, "% Conjunctive types"
		, unlines conjTypes
		, ""
		, "% Disjunctive types"
 	 	, unlines (reverse disjTypes)
		, ""
		, "% Predicates"
		, unlines conjPreds
		, unlines tokenPreds
		])

createConjPreds :: Rule Conj -> MakeTeaMonad String
createConjPreds (Conj head body) = do
	predName <- toPredName head
	typeName <- toTypeName head
	args <- forM body $ \term -> do
		argType <- elim termToTypeName term
		let argName = toVarName term
		return (argName ++ ":" ++ argType) 
	return $ if (length args == 0) 
				then "predicate " ++ predName ++ " (ID:" ++ typeName ++ ")."
				else "predicate " ++ predName ++ " (ID:" ++ typeName ++ ", " ++ flattenComma (args) ++ ")."

createTokenPreds :: Symbol Terminal -> MakeTeaMonad String
createTokenPreds (Terminal name ctype) = do
	typeName <- toTypeName (Terminal name ctype)
	predName <- toPredName (Terminal name ctype)
	return $ "predicate " ++ predName ++ " (ID:" ++ typeName ++ ", VALUE:" ++ (toClpaPrimType ctype) ++ ")."


createConjForwardDecls :: Rule Conj -> MakeTeaMonad String
createConjForwardDecls (Conj head body) = do
	typeName <- toTypeName head
	return $ "type " ++ typeName ++ "."

createDisjForwardDecls :: Rule Disj -> MakeTeaMonad String
createDisjForwardDecls (Disj head _) = do
	typeName <- toTypeName head
	return $ "type " ++ typeName ++ "."

createTokenDecls :: Symbol Terminal -> MakeTeaMonad String
createTokenDecls (Terminal name ctype) = do
	typeName <- toTypeName (Terminal name ctype)
	return $ "type " ++ typeName ++ " ::= " ++ typeName ++ "_id {id}."

toClpaPrimType :: Maybe String -> String
toClpaPrimType (Just "String*") = "string"
toClpaPrimType (Just "long") = "int"
toClpaPrimType (Just "bool") = "bool"
toClpaPrimType (Just "double") = "float"
toClpaPrimType (Just "") = "null"
toClpaPrimType Nothing = "string"

createConjTypes :: Rule Conj -> MakeTeaMonad String
createConjTypes (Conj head body) = do
	typeName <- toTypeName head
	args <- forM body $ \term -> do
		argType <- elim (termToTypeName) term
		return argType
	return $ "type " ++ typeName ++ " ::= " ++ typeName ++ "_id {id}."

filterConjTypes :: Name Class -> Rule Conj -> MakeTeaMonad Bool
filterConjTypes name (Conj head body) = do
	tn <- toTypeName head
	return (name == tn)


createDisjTypes :: Rule Disj -> MakeTeaMonad String
createDisjTypes (Disj head body) = do
	typeName <- toTypeName head
	inst <- concreteInstances head -- TODO should this be allInstances?
	body <- forM inst $ \term -> do
		tn <- toTypeName term -- TODO: remove the php from the middle here, that'll get tedious
		return $ (typeName ++ "_" ++ tn ++ "_id { " ++ tn ++ " } ")
	return $ "type " ++ typeName ++ " ::= \n\t\t  " ++ (flattenPipe (map (++ "\n\t\t" ) (filter (/= "") body))) ++ "."


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
symbolToPredName (NonTerminal n) = return ("ast_" ++ n) 
symbolToPredName (Terminal n _) = return ("ast_" ++ n) 

termToPredName :: Term NonMarker -> MakeTeaMonad CType 
termToPredName (Term _ s m) = do
	cn <- elim symbolToPredName s
	if isVector m 
		then do
			error ("unreachable")
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
termToVarName (Term (Just n) _ _) = map toUpper n
termToVarName (Marker Nothing m) = "IS_" ++ (map toUpper m)
termToVarName (Marker (Just n) _) = map toUpper n

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
symbolToTypeName (NonTerminal n) = return ("t_ast_" ++ n)
symbolToTypeName (Terminal n _) = return ("t_ast_" ++ n) 
--symbolToTypeName (NonTerminal n) = return "id"
--symbolToTypeName (Terminal n _) = return "id"

termToTypeName :: Term a -> MakeTeaMonad CType 
termToTypeName (Term _ s m) = do
	cn <- elim symbolToTypeName s
	case m of
		Vector		-> return ("list[" ++ cn ++ "]")
		Optional		-> return ("maybe[" ++ cn ++ "]")
		VectorOpt	-> return ("list[maybe[" ++ cn ++ "]]")
		OptVector	-> return ("maybe[list[" ++ cn ++ "]]")
		otherwise	-> return cn

termToTypeName m@(Marker _ _) = return "bool" 
