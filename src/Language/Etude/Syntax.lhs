Etude - Abstract Syntax
=======================

    Copyright © 2003 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

> {-# LANGUAGE OverloadedStrings #-}

> module Language.Etude.Syntax (
>   Term (..), Terms,
>   Tail (..),
>   Atom (..), Atoms,
>   Binding (..), Bindings,
>   Function (..), Functions,
> ) where

> import Language.Etude.Utilities
> import Text.PrettyPrint

> import qualified Data.Map as Map

> data Term nu =
>     RET (Tail nu)                     -- ^ lifted atoms
>   | LET (Bindings nu) (Term nu)       -- ^ monadic bindings
>   | LETREC (Functions nu) (Term nu)   -- ^ recursive (lazy) bindings
>   | IF (Atom nu) (Term nu) (Term nu)  -- ^ conditional expression
>   deriving (Eq, Ord)

> type Terms nu = List (Term nu)

> instance Show nu => Show (Term nu) where
>   showsPrec p = showsPrec p . prettyTerm

> prettyTerm :: Show nu => Term nu -> Doc
> prettyTerm (RET t)        = prettyTail t
> prettyTerm (LET bs e')    = "let" $$ indent (prettyBindings bs) $+$ prettyBodyTerm e'
> prettyTerm (LETREC fs e') = "letrec" $$ indent (prettyFunctions fs) $+$ prettyBodyTerm e'
> prettyTerm (IF a e1 e2)   = prettyConditionalTerm a e1 e2

> prettyBodyTerm :: Show nu => Term nu -> Doc
> prettyBodyTerm (RET t) = "in" <+> prettyTail t
> prettyBodyTerm e = prettyTerm e

> prettyConditionalTerm :: Show nu => Atom nu -> Term nu -> Term nu -> Doc
> prettyConditionalTerm a (RET t1) (RET t2) = "if" <+> prettyAtom a <+> "then" <+> prettyTail t1 <+> "else" <+> prettyTail t2
> prettyConditionalTerm a (RET t1) e2       = "if" <+> prettyAtom a <+> "then" <+> prettyTail t1 <+> "else" $+$ prettyTerm e2
> prettyConditionalTerm a e1 e2             = "if" <+> prettyAtom a <+> "then" $+$ indent (prettyTerm e1) $+$ "else" $+$ indent (prettyTerm e2)

> data Tail nu =
>     COPY (Atoms nu)
>   | CALL (Atom nu) (Atoms nu)
>   deriving (Eq, Ord)

> instance Show nu => Show (Tail nu) where
>   showsPrec p = showsPrec p . prettyTail

> prettyTail :: Show nu => Tail nu -> Doc
> prettyTail (COPY as) = prettyAtoms as
> prettyTail (CALL a as) = prettyAtom a <> parens (prettyAtoms as)

> data Atom nu =
>   VAR nu			-- ^ variable
>   | IMM Integer               -- ^ constant
>   deriving (Eq, Ord)

> type Atoms nu = List (Atom nu)

> instance Show nu => Show (Atom nu) where
>   showsPrec p = showsPrec p . prettyAtom
>   showList = shows . prettyAtoms

> prettyAtom :: Show nu => Atom nu -> Doc
> prettyAtom (VAR x) = prettyVar x
> prettyAtom (IMM n) = integer n

> prettyAtoms :: Show nu => Atoms nu -> Doc
> prettyAtoms = hsep . punctuate "," . map prettyAtom

> data Binding nu = BIND (List nu) (Tail nu)
>   deriving (Eq, Ord)

> type Bindings nu = List (Binding nu)

> instance Show nu => Show (Binding nu) where
>   showsPrec p = showsPrec p . prettyBinding
>   showList = shows . prettyBindings

> prettyBinding :: Show nu => Binding nu -> Doc
> prettyBinding (BIND xs t) = prettyVars xs <+> "=" <+> prettyTail t

> prettyBindings :: Show nu => Bindings nu -> Doc
> prettyBindings = vcat . map prettyBinding

> data Function nu = FUN (List nu) (Term nu)
>   deriving (Eq, Ord)

> type Functions nu = Map nu (Function nu)

> instance Show nu => Show (Function nu) where
>   showsPrec p = showsPrec p . prettyFunction
>   showList = shows . vcat . map prettyFunction

> prettyFunction :: Show nu => Function nu -> Doc
> prettyFunction (FUN xs e) = prettyPrefixedTerm ("lambda" <+> prettyVars xs <+> ".") e

> prettyFunctions :: Show nu => Functions nu -> Doc
> prettyFunctions = vcat . map (uncurry prettyFunctionBinding) . Map.toList

> prettyFunctionBinding :: Show nu => nu -> Function nu -> Doc
> prettyFunctionBinding x (FUN xs e) = prettyPrefixedTerm (prettyVar x <> parens (prettyVars xs) <+> "=") e

> prettyVar :: Show nu => nu -> Doc
> prettyVar x = text (showsPrec 11 x "")

> prettyVars :: Show nu => List nu -> Doc
> prettyVars = hsep . punctuate "," . map prettyVar

> prettyPrefixedTerm :: Show nu => Doc -> Term nu -> Doc
> prettyPrefixedTerm prefix (RET t) = prefix <+> prettyTail t
> prettyPrefixedTerm prefix e = prefix $+$ indent (prettyTerm e)

> indent = nest 4
