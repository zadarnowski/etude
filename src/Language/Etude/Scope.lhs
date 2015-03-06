Etude - Abstract Syntax
=======================

    Copyright © 2003 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

> module Language.Etude.Scope (
>   fvOfTerm, fvOfTail, fvOfAtom, fvOfAtoms,
>   fvOfBinding, fvOfBindings, fvOfFunction, fvOfFunctions,
>   bvOfBinding, bvOfBindings, bvOfFunctions
> ) where

> import Language.Etude.Syntax
> import Language.Etude.Utilities

> import qualified Data.Map as Map
> import qualified Data.Set as Set

> fvOfTerm :: Ord nu => Term nu -> Set nu
> fvOfTerm (RET t) = fvOfTail t
> fvOfTerm (LET bs e) = fvOfBindings bs `Set.union` (fvOfTerm e `Set.difference` bvOfBindings bs)
> fvOfTerm (LETREC fs e) = (fvOfFunctions fs `Set.union` fvOfTerm e) `Set.difference` bvOfFunctions fs
> fvOfTerm (IF a e1 e2) = fvOfAtom a `Set.union` fvOfTerm e1 `Set.union` fvOfTerm e2

> fvOfTail :: Ord nu => Tail nu -> Set nu
> fvOfTail (COPY as) = fvOfAtoms as
> fvOfTail (CALL a as) = fvOfAtom a `Set.union` fvOfAtoms as

> fvOfAtom :: Ord nu => Atom nu -> Set nu
> fvOfAtom (VAR nu) = Set.singleton nu
> fvOfAtom (IMM _) = Set.empty

> fvOfAtoms :: Ord nu => Atoms nu -> Set nu
> fvOfAtoms = Set.unions . map fvOfAtom

> fvOfBinding :: Ord nu => Binding nu -> Set nu
> fvOfBinding (BIND _ t) = fvOfTail t

> fvOfBindings :: Ord nu => Bindings nu -> Set nu
> fvOfBindings = Set.unions . map fvOfBinding

> fvOfFunction :: Ord nu => Function nu -> Set nu
> fvOfFunction (FUN xs e) = fvOfTerm e `Set.difference` Set.fromList xs

> fvOfFunctions :: Ord nu => Functions nu -> Set nu
> fvOfFunctions = Set.unions . map fvOfFunction . Map.elems

> bvOfBinding :: Ord nu => Binding nu -> Set nu
> bvOfBinding (BIND xs _) = Set.fromList xs

> bvOfBindings :: Ord nu => Bindings nu -> Set nu
> bvOfBindings = Set.unions . map bvOfBinding

> bvOfFunctions :: Ord nu => Functions nu -> Set nu
> bvOfFunctions = Map.keysSet
