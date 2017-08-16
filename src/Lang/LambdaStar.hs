module Lang.LambdaStar where

import Control.Lens hiding (Level, Index)
import Data.String (IsString, fromString)
import Data.Char (isUpper)

type Level = Int
type Ident = String
type Index = Int -- debrujin indices for variables

data Name = Name Level Ident
    deriving (Show, Eq)

instance IsString Name where
    fromString s
      | null s = error "empty variable name"
      | isUpper $ head s = Name 1 s
      | otherwise = Name 0 s

levelName :: Name -> Level
levelName (Name l _) = l

data Subst' = Push Name Term
            | Pop Name
            deriving (Show, Eq)

type Subst = [Subst']

_Push :: Prism' Subst' (Name, Term)
_Push = prism' put get where
    put (n, t) = Push n t
    get (Push n t) = Just (n, t)
    get (Pop _) = Nothing

ident :: Subst
ident = []

data Term = V Name Index Subst
          | Lam Name Term
          | App Level Term Term
          deriving (Show, Eq)

infixl 9 @@
(@@) :: Term -> Term -> Term
(@@) = App 0

infixl 9 @@@
(@@@) :: Term -> Term -> Term
(@@@) = App 0

instance IsString Term where fromString s = V (fromString s) 0 ident

deltaIndex :: Name -> Name -> Index
deltaIndex w v
  | w == v = 1
  | otherwise = 0

vdComponent :: Subst -> Name -> Index -> Term
vdComponent [] v d = V v d ident
vdComponent (Push w m : s) v d
  | v == w && d == 0 = m
  | otherwise = vdComponent s v (d - deltaIndex v w)
vdComponent (Pop w : s) v d = vdComponent s v (d + deltaIndex v w)

restrict :: Subst -> (Name -> Bool) -> Subst
restrict [] _ = ident
restrict (Push v m : s) np
  | np v = Push v m : s `restrict` np
  | otherwise = s `restrict` np
restrict (Pop v : s) np
  | np v = Pop v : s `restrict` np
  | otherwise = s `restrict` np

restrictNames :: Subst -> [Name] -> Subst
restrictNames s names = s `restrict` (`elem` names)

restrictLevel :: Subst -> (Level -> Bool) -> Subst
restrictLevel s lp = s `restrict` (lp . levelName)

restrictLevelLess :: Subst -> Level -> Subst
restrictLevelLess s l = s `restrictLevel` (<l)

restrictLevelGreaterEq :: Subst -> Level -> Subst
restrictLevelGreaterEq s l = s `restrictLevel` (>=l)

applySubst :: Term -> Subst -> Term
applySubst (V v d t) s
  | ident == t && ident == s' = V v d s
  | otherwise = vdComponent (s %. ts') v d
    where
        s' = s `restrictLevelGreaterEq` levelName v
        ts' = (t %. s) `restrictLevelLess` levelName v
applySubst (Lam v m) s = Lam v (m %* push v s)
    where
        push v s = Push v (V v 0 ident) : s %. (Pop v : ident)
applySubst (App l m n) s = App l (m %* s) (n %* s')
    where
        s' = s `restrictLevelGreaterEq` l

composeSubst :: Subst -> Subst -> Subst
composeSubst [] s = s
composeSubst (Push v m : t) s = Push v (m %* s') : t %. s
    where
        s' = s `restrictLevelGreaterEq` levelName v
composeSubst (Pop v : t) s = Pop v : t %. s

infixl 9 %*
infixl 9 %.

(%.) :: Subst -> Subst -> Subst
(%*) = applySubst

(%*) :: Term -> Subst -> Term
(%.) = composeSubst

heightTerm ::  Term -> Int
heightTerm (V v d s) = heightSubst s
heightTerm (Lam _ m) = heightTerm m + 1
heightTerm (App l m n) = max (heightTerm m) (heightTerm n) + 1

heightSubst :: Subst -> Int
heightSubst [] = 0
heightSubst (Push _ m : s) = max (heightTerm m + 1) (heightSubst s)
heightSubst (Pop _ : s) = heightSubst s

levelSubst :: Subst -> Level
levelSubst [] = 0
levelSubst (Push _ (V _ 0 ident) : s) = levelSubst s
levelSubst (Push v _ : s) = max (levelName v) (levelSubst s)
levelSubst (Pop _ : s) = levelSubst s

betaReduce :: Term -> Term
betaReduce (V v d s) = V v d $ over (traverse . _Push . _2) betaReduce s
betaReduce (Lam v m) = Lam v $ betaReduce m
betaReduce (App l m n) = case betaReduce m of
    (Lam v m') -> if l == levelName v
                     then betaReduce (m' %* [Push v n])
                     else App l (Lam v m') n
    m' -> App l m' (betaReduce n)
