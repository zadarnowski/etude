Etude - Utilities
=================

    Copyright © 2003 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

> module Language.Etude.Utilities (
>   List, Set, Map,
>   intersperse,
>   showForeign
> ) where

> import Data.Map (Map)
> import Data.Set (Set)
> import Data.List

> type List a = [a]

> showForeign :: Show a => a -> ShowS
> showForeign = showsPrec 11
