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
	conjForwardDecls <- withConj $ mapM createForwardDecls
	disjForwardDecls <- withDisj $ mapM createForwardDecls
	tokenDecls <- withTokens $ mapM createTokenDecls
	conjTypes <- withConj $ mapM createConjTypes
	disjTypes <- withDisj $ mapM createDisjTypes
	conjGenerics <- withConj $ mapM createConjGenerics
	disjGenerics <- withDisj $ mapM createDisjGenerics
	tokensGenerics <- withTokens $ mapM createTokenGenerics
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
		, ""
		, "% Generics"
		, ""
		, unlines conjGenerics
		, ""
		, unlines disjGenerics
		, ""
		, unlines tokensGenerics
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
createForwardDecls :: Rule a -> MakeTeaMonad String
createForwardDecls (Conj head body) = do
	typeName <- toTypeName head
	return $ "type " ++ typeName ++ "."

createForwardDecls (Disj head _) = do
	typeName <- toTypeName head
	return $ "type " ++ typeName ++ "."

createTokenDecls :: Symbol Terminal -> MakeTeaMonad String
createTokenDecls (Terminal name ctype) = do
	typeName <- toTypeName (Terminal name ctype)
	let primType = toClpaPrimType ctype
	typeConstructor <- toConstructor (Terminal name ctype)
	return $ "type " ++ typeName ++ " ::= " ++ typeConstructor ++ " { ID:id, VALUE:" ++ primType ++ " }."

{-
 - Type declarations
 -}
createConjTypes :: Rule Conj -> MakeTeaMonad String
createConjTypes (Conj head body) = do
	constructorName <- toConstructor head
	typeName <- toTypeName head
	args <- forM body $ \term -> do
		argType <- elim termToTypeName term
		arg <- toVarName term
		return (arg ++ ":" ++ argType)
	let allArgs = "ID:id":args
	return $ "type " ++ typeName ++ " ::= " ++ constructorName ++ " { " ++ (flattenWith ", " allArgs) ++ " }."


createDisjTypes :: Rule Disj -> MakeTeaMonad String
createDisjTypes (Disj head body) = do
	typeName <- toTypeName head
	baseName <- toDisjBaseName head
	inst <- concreteInstances head -- TODO should this be allInstances?
	body <- forM inst $ \term -> do
		tn <- toTypeName term
		disjName <- toDisj term baseName 
		return $ (disjName ++ " { " ++ tn ++ " } ")
	let nonBlankBody = filter (/= "") body
	return $
		"type " ++ typeName ++ " ::= \n\t\t  " 
		++ (flattenWith "\n\t\t| " nonBlankBody)
		++ "\n\t\t."



{-
 - Generics
 -}

createConjGenerics :: Rule Conj -> MakeTeaMonad String
createConjGenerics (Conj head body) = do
	constructor <- toConstructor head
	disjName <- toDisj head "node"
	args <- forM body $ \term -> do toVarName term
	let allArgs = "ID":args
	return $ 
		"to_node (any{" ++ constructor ++ "{" ++ flattenComma allArgs ++ "}},\n\t"
		++ disjName ++ "{" ++ constructor ++ "{" ++ flattenComma allArgs ++ "}}) :- .\n"
		++ "get_id (" ++ disjName ++ "{" ++ constructor ++ "{" ++ flattenComma allArgs ++ "}}, ID) :- .\n"

createDisjGenerics :: Rule Disj -> MakeTeaMonad String
createDisjGenerics (Disj head body) = do
	baseName <- toDisjBaseName head
	inst <- concreteInstances head
	body <- forM inst $ \term -> do -- TODO if it doesnt equal Node
		baseDisjName <- toDisj term baseName
		nodeDisjName <- toDisj term "node"
		constructor <- toConstructor term
		return $ 
			"to_node (any{" ++ baseDisjName ++ "{ID}}, " ++ nodeDisjName ++ "{ID}) :- ."
			-- No get_id here since we only need it for Nodes
			
	return $ if baseName == "node" then "" -- we dont need node_ rules.
				else flattenWith "\n" body

createTokenGenerics :: Symbol Terminal -> MakeTeaMonad String
createTokenGenerics t = do
	constructor <- toConstructor t 
	disjName <- toDisj t "node"
	return $  
		"to_node (any{" ++ constructor ++ "{ID, VALUE}}, " ++ disjName ++ "{" ++ constructor ++ "{ID, VALUE}}) :- .\n" ++
		"get_id (" ++ disjName ++ "{" ++ constructor ++ "{ID, _}}, ID) :- .\n"


{-
 - Visitors
 -}

createVisitor :: Symbol a -> [String] -> [String] -> [String] -> MakeTeaMonad String
createVisitor term args genArgs argGenerics = do
	let allArgs = "_":args
	constructor <- toConstructor term
	disjName <- toDisj term "node"
	typeString <- toTypeString term
	return $ 
			"to_generic (" ++ disjName ++ "{NODE}, GENERIC) :-\n\t"
			++ (flattenWith ",\n\t" ([
			"NODE = " ++ constructor ++ " { " ++ flattenComma allArgs ++ " } "
			] ++ argGenerics ++ [
			"GENERIC = gnode{" ++ disjName ++ "{NODE}, " ++ typeString ++ ", [" ++ flattenComma genArgs ++ "]}.\n"]))

createConjVisitors:: Rule Conj -> MakeTeaMonad (String)
createConjVisitors (Conj head body) = do
	argGenerics <- forM body $ \term -> do toGenericsUse term
	genArgs <- forM body $ \term -> do toGenericsName term
	args <- forM body $ \term -> do toVarName term
	createVisitor head args genArgs argGenerics 
	
createTokenVisitors :: Symbol Terminal -> MakeTeaMonad String
createTokenVisitors t@(Terminal name ctype) = do
	genericUse <- toGenericsUse t
	arg <- toVarName t
	genArg <- toGenericsName t
	createVisitor t [arg] [genArg] [genericUse] 
	


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


{- Turn types into type String: "Assign_var" -}

class ToTypeString a where
	toTypeString :: a -> MakeTeaMonad (Name Class) -- TODO Class?

instance ToTypeString (Symbol a) where
	toTypeString = symbolToTypeString

instance ToTypeString (Some Symbol) where
	toTypeString = elim symbolToTypeString

instance ToTypeString (Term a) where
	toTypeString = termToTypeString

symbolToTypeString :: Symbol a -> MakeTeaMonad (Name Class) -- TODO Class?
symbolToTypeString (NonTerminal n) = do
	return ("\"" ++ n ++ "\"")
symbolToTypeString (Terminal n _) = do
	return ("\"" ++ n ++ "\"")

termToTypeString :: Term a -> MakeTeaMonad CType 
termToTypeString (Term _ s m) = do
	cn <- elim symbolToTypeString s
	return cn



{- Turn types into constructors: assign_var -}
class ToConstructor a where
	toConstructor :: a -> MakeTeaMonad (Name Class)

instance ToConstructor (Symbol a) where
	toConstructor = symbolToConstructor

instance ToConstructor (Some Symbol) where
	toConstructor = elim symbolToConstructor

instance ToConstructor (Term NonMarker) where
	toConstructor = termToConstructor

symbolToConstructor :: Symbol a -> MakeTeaMonad (Name Class)
symbolToConstructor (NonTerminal n) = do return (checkForKeywords (lowerFirstChar n) "c")
symbolToConstructor (Terminal n _) = do return (checkForKeywords (lowerFirstChar n) "c") 

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


{- Turn types into Disj : given the first part of node_Target, produce the whole thing. -}
class ToDisj a where
	toDisj :: a -> String -> MakeTeaMonad String

instance ToDisj (Symbol a) where
	toDisj = symbolToDisj

instance ToDisj (Some Symbol) where
	toDisj = elim symbolToDisj

instance ToDisj (Term NonMarker) where
	toDisj = termToDisj

symbolToDisj :: Symbol a -> String -> MakeTeaMonad String
symbolToDisj (NonTerminal n) base = do return (base ++ "_" ++ n)
symbolToDisj (Terminal n _) base = do return (base ++ "_" ++ n)

termToDisj :: Term a -> String -> MakeTeaMonad CType 
termToDisj (Term _ s _) base = do
	cn <- elim symbolToDisj s base
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

maybeToGenericUse :: Term a -> String -> String -> MakeTeaMonad String
maybeToGenericUse t arg gen = do
	yesRule <- termToGenericsUse t
	opt <- toVarName t
	genOpt <- termToGenericsName t
	typeString <- termToTypeString t
	return $ (flattenWith "\n\t" [
			"((" ++ arg ++ " = yes{" ++ opt ++ "},"
			, yesRule ++ ","
			, gen ++ " = gmaybe{" ++ typeString ++ ", yes{" ++ genOpt ++ "}})"
			, ";"
			, "(" ++ arg ++ " \\= yes{_},"
			, gen ++ " = gmaybe{" ++ typeString ++ ", no}))"])

termToGenericsUse :: Term a -> MakeTeaMonad (Name Class)
termToGenericsUse term@(Term l s m) = do
	gen <- toGenericsName term
	arg <- toVarName term
	case m of
		Vector		-> do
			typeString <- toTypeString (Term l s Single)
			return ("list_to_generic_list (" ++ typeString ++ ", "
				++ arg ++ ", " ++ gen ++ ")")

		Optional		-> do (maybeToGenericUse (Term l s Single) arg gen)
		OptVector	-> do (maybeToGenericUse (Term l s Vector) arg gen)

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
