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
	conjToNodes <- withConj $ mapM createConjToNodes
	disjToNodes <- withDisj $ mapM createDisjToNodes
	tokensToNodes <- withTokens $ mapM createTokenToNodes
	getConjTypes <- withConj $ mapM createConjGetTypes
	getTokenTypes <- withTokens $ mapM createTokenGetTypes
	conjVisitors <- withConj $ mapM createConjVisitors
	tokenVisitors <- withTokens $ mapM createTokenVisitors
	prefix <- getPrefix
	return $ (unlines 
		[ "import \"src/analyse/base.clp\"."
		, ""
		, "session " ++ prefix ++ " ()."
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
		, "% Conjunctive Predicates"
		, unlines conjPreds
		, "% Token Predicates"
		, unlines tokenPreds
		, ""
		, ""
		, "% Generics"
		, ""
		, "% Conjunctive Type Casts"
		, unlines conjToNodes
		, ""
		, "% Disjunctive Type Casts"
		, unlines disjToNodes
		, ""
		, "% Tokens Casts"
		, unlines tokensToNodes
		, ""
		, ""
		, "% Type names"
		, unlines getConjTypes
		, unlines getTokenTypes
		, ""
		, "% Conjunctive data visitors"
		, unlines conjVisitors
		, ""
		, "% Token data visitors"
		, unlines tokenVisitors
		, ""
		])

getPrefix :: MakeTeaMonad String
getPrefix = do
	ns <- getNamespace
	let ns' = case ns of
				Nothing -> ""
				(Just name) -> map toLower name
	return ns'

lowerFirstChar :: String -> String
lowerFirstChar (n:ns) = ((toLower n):ns)

checkForKeywords :: String -> String -> String
checkForKeywords n prefix =
	let keywords = ["type", "predicate", "session", "analyze", "using", "import"]
	in 
		if (elem n keywords) then checkForKeywords (prefix ++ "_" ++ n) prefix
		else n


{-
 - IR Predicates
 -}
createConjPreds :: Rule Conj -> MakeTeaMonad String
createConjPreds (Conj head body) = do
	predName <- toPredName head
	typeName <- toTypeName head
	args <- forM body $ \term -> do
		argType <- elim termToTypeName term
		let argName = toVarName term
		return (argName ++ ":" ++ argType)
	let allArgs = ("ID:" ++ typeName):args
	return $ "predicate " ++ predName ++ " (" ++ flattenComma (allArgs) ++ ")."

createTokenPreds :: Symbol Terminal -> MakeTeaMonad String
createTokenPreds (Terminal name ctype) = do
	typeName <- toTypeName (Terminal name ctype)
	predName <- toPredName (Terminal name ctype)
	let primType = toClpaPrimType ctype
	return $ "predicate " ++ predName ++ " (ID:" ++ typeName ++ ", VALUE:" ++ primType ++ ")."

-- TODO make this more generic
toClpaPrimType :: Maybe String -> String
toClpaPrimType (Just "String*") = "string"
toClpaPrimType (Just "long") = "int"
toClpaPrimType (Just "bool") = "bool"
toClpaPrimType (Just "double") = "float"
toClpaPrimType (Just "") = "null"
toClpaPrimType Nothing = "string"


{-
 - Forward declarations
 -}
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

{-
 - Type declarations
 -}
createConjTypes :: Rule Conj -> MakeTeaMonad String
createConjTypes (Conj head body) = do
	constructorName <- toConstructor head
	typeName <- toTypeName head
	return $ "type " ++ typeName ++ " ::= " ++ constructorName ++ " { id }."

createDisjTypes :: Rule Disj -> MakeTeaMonad String
createDisjTypes (Disj head body) = do
	typeName <- toTypeName head
	baseName <- toDisjBaseName head
	inst <- concreteInstances head -- TODO should this be allInstances?
	body <- forM inst $ \term -> do
		tn <- toTypeName term
		subName <- toDisjSubName term
		return $ (baseName ++ "_" ++ subName ++ " { " ++ tn ++ " } ")
	let nonBlankBody = filter (/= "") body
	return $
		"type " ++ typeName ++ " ::= \n\t\t  " 
		++ (flattenWith "\n\t\t| " nonBlankBody)
		++ "\n\t\t."



{-
 - Generics
 -}

createConjToNodes :: Rule Conj -> MakeTeaMonad String
createConjToNodes (Conj head body) = do
	constructor <- toConstructor head
	subName <- toDisjSubName head
	return $ flattenWith "\n\t" [
		"to_node (ANY, NODE) :- ",
		"ANY = any{" ++ constructor ++ "{ID}},",
		"NODE = node_" ++ subName ++ "{" ++ constructor ++ "{ID}}.\n"]

createDisjToNodes :: Rule Disj -> MakeTeaMonad String
createDisjToNodes (Disj head body) = do
	baseName <- toDisjBaseName head
	inst <- concreteInstances head -- TODO should this be allInstances?
	body <- forM inst $ \term -> do
		subName <- toDisjSubName term
		-- TODO if it doesnt equal Node
		return $ flattenWith "\n\t" [
			"to_node (ANY, NODE) :- ",
			"ANY = any{" ++ baseName ++ "_" ++ subName ++ "{INNER}},",
			"to_node (any{INNER}, NODE)."]
-- Is there any difference with the previous syntax?
--			"ANY = any{" ++ baseName ++ "_" ++ subName ++ "{" ++ constructorName ++ "{ID}},",
--			"NODE = node_" ++ subName ++ "{" ++ constructorName ++ "{ID}}." ]
	return $ flattenWith "\n\n" body

createTokenToNodes :: Symbol Terminal -> MakeTeaMonad String
createTokenToNodes t = do
	constructor <- toConstructor t 
	subName <- toDisjSubName t
	return $ flattenWith "\n\t" [
		"to_node (ANY, NODE) :- ",
		"ANY = any{" ++ constructor ++ "{ID}},",
		"NODE = node_" ++ subName ++ "{" ++ constructor ++ "{ID}}.\n"]



{-
 - Type names
 -}
createConjGetTypes :: Rule Conj -> MakeTeaMonad String
createConjGetTypes (Conj head body) = do
	prefix <- getPrefix
	predName <- toPredName head
	typeName <- toDisjSubName head
	args <- forM body $ \term -> do
		return $ "_"
	let allArgs = "ID":args
	return $ 
		prefix ++ "()->" ++ predName ++ "(" ++ (flattenComma allArgs) ++ "), "
		++ "+get_type (node_" ++ typeName ++ "{ID}, \"" ++ typeName ++ "\")."

createTokenGetTypes :: Symbol Terminal -> MakeTeaMonad String
createTokenGetTypes t = do
	prefix <- getPrefix
	predName <- toPredName t
	typeName <- toDisjSubName t
	return $ 
		prefix ++ "()->" ++ predName ++ "(ID, _), "
		++ "+get_type (node_" ++ typeName ++ "{ID}, \"" ++ typeName ++ "\")."



{-
 - Visitors
 -}
createConjVisitors:: Rule Conj -> MakeTeaMonad String
createConjVisitors (Conj head body) = do
	prefix <- getPrefix
	predName <- toPredName head
	typeName <- toDisjSubName head
	args <- forM body $ \term -> do
		let paramName = toGenericsParamName term
		return $ paramName
	let allArgs = "ID":args
	argGenerics <- forM body $ \term -> do
		generics <- toGenericsUse term
		return $ generics
	genArgs <- forM body $ \term -> do
		names <- toGenericsName term
		return $ names
	return $ flattenWith ",\n\t" ([
		prefix ++ "()->" ++ predName ++ "(" ++ flattenComma allArgs ++ ")"
		, "to_node (any{ID}, NODE)"
		] ++ argGenerics ++ [
		"GENERIC = gnode{NODE, [" ++ flattenComma genArgs ++ "]}"
		, "+to_generic (NODE, GENERIC).\n"])

createTokenVisitors :: Symbol Terminal -> MakeTeaMonad String
createTokenVisitors (Terminal name ctype) = do
	let t = (Terminal name ctype)
	prefix <- getPrefix
	predName <- toPredName t
	genericUse <- toGenericsUse t
	typeNameUse <- toDisjSubName t
	let arg = toGenericsParamName t
	genArg <- toGenericsName t
	return $ flattenWith ",\n\t" [
		  prefix ++ "()->" ++ predName ++ "(ID, " ++ arg ++ ")"
		, "to_node (any{ID}, NODE)"
		, genericUse
		, "GENERIC = gnode{NODE, [" ++ genArg ++ "]}"
		, "+to_generic (NODE, GENERIC).\n"]



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


{- Turn types into a use of the to_generic rules -}
class ToGenericsUse a where
	toGenericsUse :: a -> MakeTeaMonad (Name Class)

instance ToGenericsUse (Term a) where
	toGenericsUse = termToGenericsUse

instance ToGenericsUse (Some Term) where
	toGenericsUse = elim termToGenericsUse

instance ToGenericsUse (Symbol a) where
	toGenericsUse = symbolToGenericsUse

instance ToGenericsUse (Some Symbol) where
	toGenericsUse = elim symbolToGenericsUse

termToGenericsUse :: Term a -> MakeTeaMonad (Name Class)
termToGenericsUse (Term l s m) = do
	genName <- toGenericsName (Term l s m)
	let paramName = toGenericsParamName (Term l s m)
	case m of
		Vector		-> do
			predName <- elim symbolToPredName s
			return ("list_to_generic_list (" 
				++ paramName ++ ", " ++ genName ++ ")")

		Optional		-> do
			let optName = toGenericsParamName (Term l s Single)
			yesRule <- termToGenericsUse (Term l s Single)
			genOptName <- termToGenericsName (Term l s Single)
			return (
				"("
				++ "(" ++ paramName ++ " = yes{" ++ optName ++ "},\n\t"
				++ yesRule ++ ",\n\t"
				++ genName ++ " = gmaybe{yes{" ++ genOptName ++ "}})\n\t"
				++ ";\n\t"
				++ "(" ++ genName ++ " = gmaybe{no}))")


		OptVector -> do
			let optName = toGenericsParamName (Term l s Vector)
			yesRule <- termToGenericsUse (Term l s Vector)
			genOptName <- termToGenericsName (Term l s Vector)
		
			return (
				"("
				++ "(" ++ paramName ++ " = yes{" ++ optName ++ "},\n\t"
				++ yesRule ++ ",\n\t"
				++ genName ++ " = gmaybe{yes{" ++ genOptName ++ "}})\n\t"
				++ ";\n\t"
				++ "(" ++ genName ++ " = gmaybe{no}))")
	
		otherwise	-> return (
			"to_node (any{" ++ paramName ++ "}, NODE_" ++ paramName ++ "),\n"
			++ "\tto_generic (NODE_" ++ paramName ++ ", " ++ genName ++ ")")
	

termToGenericsUse (Marker l m) = do
	let name = toGenericsParamName (Marker l m)
	genName <- toGenericsName (Marker l m)
	return (genName ++ " = gmarker {" ++ name ++ "}")

symbolToGenericsUse :: Symbol a -> MakeTeaMonad (Name Class)
symbolToGenericsUse (NonTerminal n) = do
	return (map toUpper n)
symbolToGenericsUse (Terminal n ctype) = do
	genName <- toGenericsName (Terminal n ctype)
	let name = toGenericsParamName (Terminal n ctype)
	let gentype = toClpaPrimType ctype
	return (genName ++ " = g" ++ gentype ++ " {" ++ name ++ "}")




{- Variable names for generics -}
class ToGenericsName a where
	toGenericsName :: a -> MakeTeaMonad (Name Class)

instance ToGenericsName (Term a) where
	toGenericsName = termToGenericsName

instance ToGenericsName (Some Term) where
	toGenericsName = elim termToGenericsName

instance ToGenericsName (Symbol a) where
	toGenericsName = symbolToGenericsName

instance ToGenericsName (Some Symbol) where
	toGenericsName = elim symbolToGenericsName

termToGenericsName :: Term a -> MakeTeaMonad (Name Class)
termToGenericsName t = do
	return ("GEN_" ++ (toGenericsParamName t))

symbolToGenericsName :: Symbol a -> MakeTeaMonad (Name Class)
symbolToGenericsName s = do
	return ("GEN_" ++ (toGenericsParamName s))


{- Parameter names for generic functions -}
class ToGenericsParamName a where
	toGenericsParamName :: a -> Name Variable 

instance ToGenericsParamName (Term a) where
	toGenericsParamName = termToGenericsParamName

instance ToGenericsParamName (Some Term) where
	toGenericsParamName = elim termToGenericsParamName

instance ToGenericsParamName (Symbol a) where
	toGenericsParamName = symbolToGenericsParamName

instance ToGenericsParamName (Some Symbol) where
	toGenericsParamName = elim symbolToGenericsParamName

termToGenericsParamName :: Term a -> Name Variable 
termToGenericsParamName (Term Nothing s m) =
	case m of
		Vector		-> name ++ "S"
		Optional		-> "OPT_" ++ name
		OptVector	-> "OPT_" ++ name ++ "S"
		VectorOpt	-> name ++ "S"
		otherwise	-> name
	where name = elim symbolToGenericsParamName s

termToGenericsParamName (Term (Just l) s m) =
	case m of
		Vector		-> name ++ "S"
		Optional		-> "OPT_" ++ name
		OptVector	-> "OPT_" ++ name ++ "S"
		VectorOpt	-> name ++ "S"
		otherwise	-> name
	where name = map toUpper l

termToGenericsParamName (Marker Nothing m) = "IS_" ++ (map toUpper m)
termToGenericsParamName (Marker (Just n) _) = map toUpper n

symbolToGenericsParamName :: Symbol a -> Name Variable 
symbolToGenericsParamName (NonTerminal n) = map toUpper n
symbolToGenericsParamName (Terminal n _) = map toUpper n

