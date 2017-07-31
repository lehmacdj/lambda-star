{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.LambdaStar.Terms where

import Prelude hiding (succ, pred, plus, minus, snd, fst)

import Lang.LambdaStar

zero :: Term
zero = Lam "s" (Lam "z" "z")

succ :: Term
succ = Lam "n" (Lam "s" (Lam "z" ("s" @@ ("n" @@ "s" @@ "z"))))

fst :: Term
fst = Lam "a" (Lam "b" "a")

snd :: Term
snd = Lam "a" (Lam "b" "b")

pred :: Term
pred = Lam "n" $ "n" @@
    (Lam "p" $ Lam "c" $ "c"
        @@ ("p" @@ snd)
        @@ (succ @@ ("p" @@ snd)))
    @@ (Lam "c" $ "c"
        @@ zero
        @@ zero)
    @@ fst

plus :: Term
plus = Lam "m" $ Lam "n" $ "n" @@ succ @@ "m"

minus :: Term
minus = Lam "m" $ Lam "n" $ "n" @@ pred @@ "m"

block :: Term
block = Lam "Y" $ Lam "x" $ "x" @@@ "Y"

unblock :: Term
unblock = Lam "Z" $ "Z" @@@ Lam "Y" "Y"
