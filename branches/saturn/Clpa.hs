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
	tokenDecls <- withTokens $ mapM createTokenDecls
	conjTypes <- withConj $ mapM createConjTypes
	disjTypes <- withDisj $ mapM createDisjTypes
	conjPreds <- withConj $ mapM createConjPreds
	tokenPreds <- withTokens $ mapM createTokenPreds
	conjToNodes <- withConj $ mapM createConjToNodes
	disjToNodes <- withDisj $ mapM createDisjToNodes
	tokensToNodes <- withTokens $ mapM createTokenToNodes
	conjVisitors <- withConj $ mapM createConjVisitors
	tokenVisitors <- withTokens $ mapM createTokenVisitors
	prefix <- getPrefix
	return $ (unlines 
		[ "import \"src/analyse/base.clp\"."
		, ""
		, "session " ++ prefix ++ " ()."
		, ""
		, "% Forward declarations"
		, unlines conjForwardDecls
		, ""
		, unlines disjForwardDecls
		, ""
		, ""
		, "% Token declarations"
		, unlines tokenDecls
		, ""
		, ""
		, "% Types"
		, unlines conjTypes
		, ""
 	 	, unlines (reverse disjTypes)
		, ""
		, ""
		, "% Predicates"
		, unlines conjPreds
		, ""
		, unlines tokenPreds
		, ""
		, ""
		, "% Generics"
		, ""
		, "% Type Casts"
		, unlines conjToNodes
		, ""
		, unlines disjToNodes
		, ""
		, unlines tokensToNodes
		, ""
		, ""
		, "% Data visitors"
		, unlines conjVisitors
		, ""
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
		arg <- toVarName term
		return (arg ++ ":" ++ argType)
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
	return $ 
		"to_node (any{" ++ constructor ++ "{ID}}, node_" ++ subName ++ "{" ++ constructor ++ "{ID}}) :- ."

createDisjToNodes :: Rule Disj -> MakeTeaMonad String
createDisjToNodes (Disj head body) = do
	baseName <- toDisjBaseName head
	inst <- concreteInstances head
	body <- forM inst $ \term -> do -- TODO if it doesnt equal Node
		subName <- toDisjSubName term
		constructor <- toConstructor term
		return $ 
			"to_node (any{" ++ baseName ++ "_" ++ subName ++ "{ID}}, node_" ++ subName ++ "{ID}) :- ."
			
	return $ if baseName == "node" then "" -- we dont need node_ rules.
				else flattenWith "\n" body

createTokenToNodes :: Symbol Terminal -> MakeTeaMonad String
createTokenToNodes t = do
	constructor <- toConstructor t 
	subName <- toDisjSubName t
	return $ 
		"to_node (any{" ++ constructor ++ "{ID}}, node_" ++ subName ++ "{" ++ constructor ++ "{ID}}) :- ."


{-
 - Visitors
 -}
createConjVisitors:: Rule Conj -> MakeTeaMonad String
createConjVisitors (Conj head body) = do
	prefix <- getPrefix
	predName <- toPredName head
	typeName <- toDisjSubName head
	argGenerics <- forM body $ \term -> do toGenericsUse term
	genArgs <- forM body $ \term -> do toGenericsName term
	args <- forM body $ \term -> do toVarName term
	typeName <- toDisjSubName head
	let allArgs = "ID":args
	return $ "to_generic (NODE, GENERIC) :-\n\t" ++ (flattenWith ",\n\t" ([
		  prefix ++ "()->" ++ predName ++ "(" ++ (flattenComma allArgs) ++ ")"
		, "to_node (any{ID}, NODE)"
		] ++ argGenerics ++ [
		"GENERIC = gnode{NODE, \"" ++ typeName ++ "\", [" ++ flattenComma genArgs ++ "]}.\n"]))

createTokenVisitors :: Symbol Terminal -> MakeTeaMonad String
createTokenVisitors (Terminal name ctype) = do
	let t = (Terminal name ctype)
	prefix <- getPrefix
	predName <- toPredName t
	genericUse <- toGenericsUse t
	arg <- toVarName t
	genArg <- toGenericsName t
	typeName <- toDisjSubName t 
	return $ "to_generic (NODE, GENERIC) :-\n\t" ++ flattenWith ",\n\t" [
		  prefix ++ "()->" ++ predName ++ "(ID, " ++ arg ++ ")"
		, "to_node (any{ID}, NODE)"
		, genericUse
		, "GENERIC = gnode{NODE, \"" ++ typeName ++ "\", [" ++ genArg ++ "]}.\n"]



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
	return cn


{- Turn types into Predicate argument names -}
class ToVarName a where
	toVarName :: a -> MakeTeaMonad (Name Class)

instance ToVarName (Term a) where
	toVarName = termToVarName

instance ToVarName (Some Term) where
	toVarName = elim termToVarName

instance ToVarName (Symbol a) where
	toVarName = symbolToVarName

instance ToVarName (Some Symbol) where
	toVarName = elim symbolToVarName

termToVarName :: Term a -> MakeTeaMonad (Name Class)
termToVarName (Term Nothing s m) = do
	name <- elim symbolToVarName s
	case m of
		Vector		-> return $ name ++ "S"
		Optional		-> return $ "OPT_" ++ name
		OptVector	-> return $ "OPT_" ++ name ++ "S"
		VectorOpt	-> return $ name ++ "S"
		otherwise	-> return $ name

termToVarName (Term (Just l) s m) = do
	name <- symbolToVarName (NonTerminal l)
	case m of
		Vector		-> return $ name ++ "S"
		Optional		-> return $ "OPT_" ++ name
		OptVector	-> return $ "OPT_" ++ name ++ "S"
		VectorOpt	-> return $ name ++ "S"
		otherwise	-> return $ name

termToVarName (Marker Nothing m) = do 
	return $ "IS_" ++ (map toUpper m)
termToVarName (Marker (Just n) _) = do 
	return $ map toUpper n

symbolToVarName :: Symbol a -> MakeTeaMonad (Name Class)
symbolToVarName (NonTerminal n) = do 
	return $ map toUpper n
symbolToVarName (Terminal n _) = do 
	return $ map toUpper n



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


{- Turn types into constructors: assign_var_id -}
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
termToConstructor (Term _ s _) = do
	cn <- elim symbolToConstructor s
	return cn



{- Turn types into DisjBaseNames: the first part of node_Target -}
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
termToDisjBaseName (Term _ s _) = do
	cn <- elim symbolToDisjBaseName s
	return cn


{- Turn types into DisjSubNames: the second part of node_Target. -}
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
termToDisjSubName (Term _ s _) = do
	cn <- elim symbolToDisjSubName s
	return cn


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
termToGenericsUse term@(Term l s m) = do
	gen <- toGenericsName term
	arg <- toVarName term
	case m of
		Vector		-> do
			typeName <- toDisjSubName (Term l s Single)
			return ("list_to_generic_list (\"" ++ typeName ++ "\", "
				++ arg ++ ", " ++ gen ++ ")")

		-- TODO merge to avoid duplicate code
		Optional		-> do
			let t = (Term l s Single)
			yesRule <- termToGenericsUse t
			opt <- toVarName t
			genOpt <- termToGenericsName t
			typeName <- toDisjSubName t
			return (
				"("
					++ "(" ++ arg ++ " = yes{" ++ opt ++ "},\n\t"
					++ yesRule ++ ",\n\t"
					++ gen ++ " = gmaybe{\"" ++ typeName ++ "\", yes{" ++ genOpt ++ "}})\n\t"
				++ ";\n\t"
					++ "(" ++ arg ++ " \\= yes{_},\n\t"
					++ "" ++ gen ++ " = gmaybe{\"" ++ typeName ++ "\", no})"
				++ ")")


		OptVector -> do
			let t = (Term l s Vector)
			yesRule <- termToGenericsUse t
			opt <- toVarName t
			genOpt <- termToGenericsName t
			typeName <- toDisjSubName t
		
			return (
				"("
					++ "(" ++ arg ++ " = yes{" ++ opt ++ "},\n\t"
					++ yesRule ++ ",\n\t"
					++ gen ++ " = gmaybe{\"" ++ typeName ++ "\", yes{" ++ genOpt ++ "}})\n\t"
				++ ";\n\t"
					++ "(" ++ arg ++ " \\= yes{_},\n\t"
					++ "" ++ gen ++ " = gmaybe{\"" ++ typeName ++ "\", no})"
				++ ")")
	
		otherwise	-> return (
			"to_node (any{" ++ arg ++ "}, NODE_" ++ arg ++ "),\n"
			++ "\tto_generic (NODE_" ++ arg ++ ", " ++ gen ++ ")")
	


termToGenericsUse mark@(Marker Nothing m) = do 
	arg <- toVarName mark
	gen <- toGenericsName mark
	return (gen ++ " = gmarker {\"is_" ++ m ++ "\", " ++ arg ++ "}")

termToGenericsUse mark@(Marker (Just n) _) = do 
	arg <- toVarName mark
	gen <- toGenericsName mark
	return (gen ++ " = gmarker {\"" ++ n ++ "\", " ++ arg ++ "}")


symbolToGenericsUse :: Symbol a -> MakeTeaMonad (Name Class)
symbolToGenericsUse (NonTerminal n) = do
	return (map toUpper n)
symbolToGenericsUse t@(Terminal n ctype) = do
	gen <- toGenericsName t
	arg <- toVarName t
	let gentype = toClpaPrimType ctype
	return (gen ++ " = g" ++ gentype ++ " {" ++ arg ++ "}")




{- "GEN_" ++ predicate argument name -}
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
	argName <- toVarName t
	return ("GEN_" ++ argName)

symbolToGenericsName :: Symbol a -> MakeTeaMonad (Name Class)
symbolToGenericsName s = do
	argName <- toVarName s
	return ("GEN_" ++ argName)
