{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 -}

module PrettyPrinter where

import Text.PrettyPrint
import DataStructures
import Util

{-
 - Grammar
 -}

instance Show (Some Rule) where
	show = elim showRule

instance Show (Rule a) where
	show = showRule

showRule :: Rule a -> String
showRule (Disj nt ss) = show nt ++ " ::= " ++ foldr1 f (map show ss) ++ ";"
	where
		f x y = x ++ " | " ++ y
showRule (Conj nt ts) = show nt ++ " ::= " ++ foldr f "" (map show ts) ++ ";"
	where
		f x y = x ++ " " ++ y

instance Show (Some Symbol) where
	show = elim showSymbol

instance Show (Symbol a) where
	show = showSymbol

showSymbol :: Symbol a -> String
showSymbol (NonTerminal n) = n
showSymbol (Terminal n (Just ctype)) = n ++ "<" ++ ctype ++ ">"
showSymbol (Terminal n Nothing) = n 

instance Show (Some Term) where
	show = elim showTerm

instance Show (Term a) where
	show = showTerm

showTerm :: Term a -> String
showTerm (Term Nothing s m) = show s ++ show m
showTerm (Term (Just l) s m) = l ++ ":" ++ show s ++ show m
showTerm (Marker Nothing m) = "\"" ++ m ++ "\"?"
showTerm (Marker (Just l) m) = l ++ ":\"" ++ m ++ "\""

instance Show Multiplicity where
	show Single = ""
	show Optional = "?"
	show Vector = "*"
	show VectorOpt = "?*"
	show OptVector = "*?"

{-
 - C++ classes
 -}

instance Show Class where
	show c = showClassHeader c ++ showClassImplementation c

showClassHeader :: Class -> String
showClassHeader c = render $
	text "class" <+> text (name c) <> docExtends (extends c) $+$
	text "{" $+$ vcat (map docSectionHeader (sections c)) $+$ vcat (map docFriend (friends c)) $+$ text "};" $+$ text ""
	
docExtends :: [Name Class] -> Doc
docExtends [] = empty
docExtends cs = text " : " <> (commaSep $ map (\c -> text "virtual public" <+> text c) cs)

docSectionHeader :: Section -> Doc
docSectionHeader (Section _ _ []) = empty
docSectionHeader (Section cmnt m ms) = docCmnt cmnt $+$ docAccess m <> colon $+$ nest 4 (vcat (map docMemberSignature ms))

docMemberSignature :: Member -> Doc
docMemberSignature (Attribute cmnt decl) = docCmnt cmnt $+$ 
	docDecl decl <> semi
docMemberSignature (Method cmnt virtual static decl args body) = docCmnt cmnt $+$ 
	docVirtual virtual <> docStatic static <>
	docDecl decl <> parens (commaSep (map docDecl args)) <> semi
docMemberSignature (PureVirtual cmnt decl args) = docCmnt cmnt $+$
	text "virtual" <+> 
	docDecl decl <> parens (commaSep (map docDecl args)) <+> text "= 0;"

docDecl :: Decl a -> Doc
docDecl (name, ctype) = text name <+> text ctype

docVirtual :: IsVirtual -> Doc
docVirtual Virtual = text "virtual "
docVirtual NonVirtual = text ""

docStatic :: IsStatic -> Doc
docStatic Static = text "static "
docStatic NonStatic = text ""

docCmnt :: Comment -> Doc 
docCmnt = vcat . map (\c -> text "//" <+> text c)

docAccess :: Access -> Doc
docAccess Private = text "private"
docAccess Protected = text "protected"
docAccess Public = text "public"

docFriend :: Name Class -> Doc
docFriend n = text "friend class" <+> text n <> semi 

showClassImplementation :: Class -> String
showClassImplementation c = render . vcat $ map f (sections c)
	where
		f :: Section -> Doc
		f (Section cmnt _ ms) 
			= docCmnt cmnt $+$ (vcat $ map (docMethod (name c)) ms)

docMethod :: Name Class -> Member -> Doc
docMethod cn (Method cmnt _ _ (ret,name) args body) = 
	docCmnt cmnt $+$
	text ret <+> text (cn ++ "::" ++ name) 
	<> parens (commaSep (map docDecl args))
	$+$ text "{" $+$ nest 4 (docBody body) $+$ text "}" $+$ text ""
docMethod cn _ = empty 

docBody :: Body -> Doc
docBody = vcat . map text

{-
 - Util
 -}

commaSep :: [Doc] -> Doc
commaSep = hsep . punctuate comma
