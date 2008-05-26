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
	prefix <- getPrefix
	return $ (unlines 
		[ "import \"src/analyse/base.clp\"."
		, ""
		, "session " ++ prefix ++ " (PROG:string)."
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

getPrefix :: MakeTeaMonad String
getPrefix = do
	ns <- getNamespace
	let ns' = case ns of
				Nothing -> ""
				(Just name) -> map toLower name
	return ns'

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
	typeConstructor <- toConstructor (Terminal name ctype)
	return $ "type " ++ typeName ++ " ::= " ++ typeConstructor ++ " { id }."

toClpaPrimType :: Maybe String -> String
toClpaPrimType (Just "String*") = "string"
toClpaPrimType (Just "long") = "int"
toClpaPrimType (Just "bool") = "bool"
toClpaPrimType (Just "double") = "float"
toClpaPrimType (Just "") = "null"
toClpaPrimType Nothing = "string"

createConjTypes :: Rule Conj -> MakeTeaMonad String
createConjTypes (Conj head body) = do
	constructorName <- toConstructor head
	typeName <- toTypeName head
	args <- forM body $ \term -> do
		argType <- elim (termToTypeName) term
		return argType
	return $ "type " ++ typeName ++ " ::= " ++ constructorName ++ " { id }."

filterConjTypes :: Name Class -> Rule Conj -> MakeTeaMonad Bool
filterConjTypes name (Conj head body) = do
	tn <- toTypeName head
	return (name == tn)

lowerFirstChar :: String -> String
lowerFirstChar (n:ns) = ((toLower n):ns)

checkForKeywords :: String -> String -> String
checkForKeywords n prefix =
	let keywords = ["type", "predicate", "session", "analyze", "using", "import"]
	in 
		if (elem n keywords) then checkForKeywords (prefix ++ "_" ++ n) prefix
		else n

createDisjTypes :: Rule Disj -> MakeTeaMonad String
createDisjTypes (Disj head body) = do
	typeName <- toTypeName head
	baseName <- toDisjBaseName head
	inst <- concreteInstances head -- TODO should this be allInstances?
	body <- forM inst $ \term -> do
		tn <- toTypeName term
		subName <- toDisjSubName term
		return $ (baseName ++ "_" ++ subName ++ " { " ++ tn ++ " } ")
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
symbolToPredName (NonTerminal n) = do
	return (checkForKeywords (lowerFirstChar (n)) "p")
symbolToPredName (Terminal n _) = do
	return (checkForKeywords (lowerFirstChar (n)) "p")




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
symbolToTypeName (NonTerminal n) = do return ("t_" ++ n)
symbolToTypeName (Terminal n _) = do return ("t_" ++ n) 

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


{- Turn types into constructors -}
class ToConstructor a where
	toConstructor :: a -> MakeTeaMonad (Name Class)

instance ToConstructor (Symbol a) where
	toConstructor = symbolToConstructor

instance ToConstructor (Some Symbol) where
	toConstructor = elim symbolToConstructor

instance ToConstructor (Term NonMarker) where
	toConstructor = termToConstructor

symbolToConstructor :: Symbol a -> MakeTeaMonad (Name Class)
symbolToConstructor (NonTerminal n) = do return (checkForKeywords (lowerFirstChar (n ++ "_id")) "t")
symbolToConstructor (Terminal n _) = do return (checkForKeywords (lowerFirstChar (n ++ "_id")) "t") 

termToConstructor :: Term a -> MakeTeaMonad CType 
termToConstructor (Term _ s m) = do
	cn <- elim symbolToConstructor s
	case m of
		Vector		-> return ("list[" ++ cn ++ "]")
		Optional		-> return ("maybe[" ++ cn ++ "]")
		VectorOpt	-> return ("list[maybe[" ++ cn ++ "]]")
		OptVector	-> return ("maybe[list[" ++ cn ++ "]]")
		otherwise	-> return cn

termToConstructor m@(Marker _ _) = return "bool" 


{- Turn types into DisjBaseNames -}
class ToDisjBaseName a where
	toDisjBaseName :: a -> MakeTeaMonad (Name Class)

instance ToDisjBaseName (Symbol a) where
	toDisjBaseName = symbolToDisjBaseName

instance ToDisjBaseName (Some Symbol) where
	toDisjBaseName = elim symbolToDisjBaseName

instance ToDisjBaseName (Term NonMarker) where
	toDisjBaseName = termToDisjBaseName

symbolToDisjBaseName :: Symbol a -> MakeTeaMonad (Name Class)
symbolToDisjBaseName (NonTerminal n) = do return (lowerFirstChar n)
symbolToDisjBaseName (Terminal n _) = do return (lowerFirstChar n) 

termToDisjBaseName :: Term a -> MakeTeaMonad CType 
termToDisjBaseName (Term _ s m) = do
	cn <- elim symbolToDisjBaseName s
	case m of
		Vector		-> return ("list[" ++ cn ++ "]")
		Optional		-> return ("maybe[" ++ cn ++ "]")
		VectorOpt	-> return ("list[maybe[" ++ cn ++ "]]")
		OptVector	-> return ("maybe[list[" ++ cn ++ "]]")
		otherwise	-> return cn

termToDisjBaseName m@(Marker _ _) = return "bool" 


{- Turn types into DisjSubNames -}
class ToDisjSubName a where
	toDisjSubName :: a -> MakeTeaMonad (Name Class)

instance ToDisjSubName (Symbol a) where
	toDisjSubName = symbolToDisjSubName

instance ToDisjSubName (Some Symbol) where
	toDisjSubName = elim symbolToDisjSubName

instance ToDisjSubName (Term NonMarker) where
	toDisjSubName = termToDisjSubName

symbolToDisjSubName :: Symbol a -> MakeTeaMonad (Name Class)
symbolToDisjSubName (NonTerminal n) = do return (n)
symbolToDisjSubName (Terminal n _) = do return (n) 

termToDisjSubName :: Term a -> MakeTeaMonad CType 
termToDisjSubName (Term _ s m) = do
	cn <- elim symbolToDisjSubName s
	case m of
		Vector		-> return ("list[" ++ cn ++ "]")
		Optional		-> return ("maybe[" ++ cn ++ "]")
		VectorOpt	-> return ("list[maybe[" ++ cn ++ "]]")
		OptVector	-> return ("maybe[list[" ++ cn ++ "]]")
		otherwise	-> return cn

termToDisjSubName m@(Marker _ _) = return "bool" 
