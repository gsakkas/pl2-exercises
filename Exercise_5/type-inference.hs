{-# OPTIONS_gHC -O2 -optc-O2 #-}

import Data.Char
import System.IO
import Text.Read
import Data.Maybe
import Control.Monad

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq
type Constraints = [(Type, Type)]
type Rules = [(Type, Type)]
type Env = [(String, Int)]

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Creating constraints

createConstraints :: Expr -> Env -> Int -> Maybe (Type, Constraints, Int)
createConstraints (Evar x) g vars =
    lookup x g >>= \t -> Just (Tvar t, [], vars)
createConstraints (Eabs x e) g vars =
    let (t, c, vars') = fromMaybe ((Tvar (-1)), [], -1) (createConstraints e ((x, vars + 1):g) (vars + 1))
    in if vars' < 0
        then Nothing
        else Just ((Tfun (Tvar (vars + 1)) t), c, vars')
createConstraints (Eapp e1 e2) g vars =
    let (t, c1, vars') = fromMaybe (Tvar (-1), [], -1) (createConstraints e1 g vars)
        (t', c2, vars'') = fromMaybe (Tvar (-1), [], -1) (createConstraints e2 g vars')
    in if vars' < 0 || vars'' < 0
        then Nothing
        else Just (Tvar (vars'' + 1), (t, Tfun t' (Tvar (vars'' + 1))):(c1 ++ c2), vars'')

-- Finds if a Type appears inside another one

appearsAt :: Type -> Type -> Bool
appearsAt t1 t2@(Tvar a) = t1 == t2
appearsAt t1 t2@(Tfun t21 t22) =
    (appearsAt t1 t21) || (appearsAt t1 t22)

-- Replaces all occurrences of the first Type with the second Type in the Constraints
replaceWith :: Type -> Type -> Constraints -> Constraints -> Constraints
replaceWith _ _ [] c2 = c2
replaceWith t1 t2 ((t1', t2'):c1) c2 = replaceWith t1 t2 c1 ((t1'', t2''):c2)
        where t1'' = replace t1 t2 t1'
              t2'' = replace t1 t2 t2'
              replace t1 t2 t'@(Tvar a)
                | t1 == t' = t2
                | otherwise = t'
              replace t1 t2 (Tfun t21 t22) =
                   Tfun (replace t1 t2 t21) (replace t1 t2 t22)

-- W-algorithm

unify :: Constraints -> Rules -> Maybe Rules
unify [] subs = Just subs
unify ((t1, t2):c) subs
    | t1 == t2 = unify c subs
unify ((t1@(Tvar a), t2):c) subs
    | not (t1 `appearsAt` t2) = unify (replaceWith t1 t2 c []) ((t1, t2):subs)
unify ((t1, t2@(Tvar a)):c) subs
    | not (t2 `appearsAt` t1) = unify (replaceWith t2 t1 c []) ((t2, t1):subs)
unify ((t1@(Tfun t11 t12), t2@(Tfun t21 t22)):c) subs =
    unify ((t11, t21):(t12, t22):c) subs
unify _ _ = Nothing

-- Applies Type Substitution
substitute :: Type -> Rules -> Type
substitute t@(Tvar a) c =
    case lookup t c of
        Just t' -> replace t t' t
        Nothing -> t
    where replace t1 t2 t'@(Tvar a)
            | t1 == t' = t2
            | otherwise = t'
          replace t1 t2 (Tfun t21 t22) =
               Tfun (replace t1 t2 t21) (replace t1 t2 t22) 
substitute t@(Tfun t1 t2) c =
    Tfun (substitute t1 c) (substitute t2 c)

-- Sorts output types
sortTypes :: Type -> Maybe Type
sortTypes t = Just (substitute t (fst sorted_types))
    where sorted_types = createSortedList t 0 []
          createSortedList t@(Tvar a) counter subs =
            case lookup t subs of
                Just t -> (subs, counter)
                Nothing -> (((t, Tvar counter):subs), counter + 1)
          createSortedList t@(Tfun t1 t2) counter subs =
            let (subs1, counter1) = createSortedList t1 counter subs
                (subs2, counter2) = createSortedList t2 counter1 subs1
            in (subs2, counter2)


-- Main program

findType =  do  s <- getLine
                let e = read s :: Expr
                let typ = createConstraints e [] 0 >>=
                           \(t, c, _) -> unify c [] >>=
                           \c -> return (substitute t c) >>=
                           \t -> sortTypes t
                maybe (putStrLn "type error") print typ

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                count n findType