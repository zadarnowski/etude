Etude - Abstract Syntax
=======================

    Copyright © 2003 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

> module Language.Etude.Syntax (
>   Term (..), Terms,
>   Tail (..),
>   Atom (..), Atoms,
>   Binding (..), Bindings,
>   Function (..), Functions,
> ) where

> import Language.Etude.Utilities

> data Term nu =
>     RETURN (Tail nu)                  -- ^ lifted atoms
>   | LET (Bindings nu) (Term nu)       -- ^ monadic bindings
>   | LETREC (Functions nu) (Term nu)   -- ^ recursive (lazy) bindings
>   | IF (Atom nu) (Term nu) (Term nu)  -- ^ conditional expressions
>   deriving (Eq, Ord)

> type Terms nu = List (Term nu)

> instance Show nu => Show (Term nu) where
>   showsPrec _ = showTerm ""
>
> showTerm px e =
>   case e in
>     RETURN t      -> showsPrec t
>     LET bs e      -> showString "let " . showBindings px' bs . showLineBreak . showTerm' px e
>     LETREC fs e   -> showString "letrec\n" . showString px' . showFunctions px' bs . showLineBreak . showTerm' px e
>     IF a (RETURN t1) (RETURN t2) -> showString "if " . shows a . showString " then " . shows t1 . showString " else " . shows t2
>     IF a (RETURN t1) e2 -> showString "if " . shows a . showString " then " . shows t1 . showString " else" . showLineBreak shows t2

>   where
>     showTerm' px t =
>       case e in
>         RETURN t -> showString "in " . shows t
>         _ -> showTerm px e
>     showLineBreak = showString ('\n' : px)
>     px' = "    " ++ px
>     showTerm' px e =
>

> data Tail nu =
>     COPY (Atoms nu)
>   | CALL (Atom nu) (Atoms nu)
>   deriving (Eq, Ord)

> data Atom nu =
>   VAR nu			-- ^ variables
>   | IMM Integer               -- ^ constants
>   deriving (Eq, Ord)

> type Atoms nu = List (Atom nu)

> data Binding nu = BIND (List nu) (Tail nu)
>   deriving (Eq, Ord)

> type Bindings nu = List (Binding nu)

> data Function nu =
>   FUN (List nu) (Term nu)
>   deriving (Eq, Ord)

> type Functions nu = Map nu (Function nu)

