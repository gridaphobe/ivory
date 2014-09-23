{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Uint8 where

import Prelude hiding (sum)
import Ivory.Language

{-@ embed Uint8 as int @-}
{-@ invariant {v:Uint8 | Btwn 0 v 256} @-}

{-@ predicate Btwn Lo N Hi = Lo <= N && N < Hi @-}

{-@ addUint8 :: x:_ -> y:{_ | Btwn 0 (x+y) 256} -> {v:_ | v = x + y} @-}
addUint8 :: Uint8 -> Uint8 -> Uint8
addUint8 = (+)

type Arr = (:->)

type Cons = '(:)
type Nil = '[]

{-@ double :: Def (Arr (Cons {v:Uint8 | Btwn 0 v 128} Nil) Uint8) @-}
double :: Def ('[Uint8] :-> Uint8)
double = proc "double" foo -- (\x -> foo x)
  where
    {- foo :: {v:Uint8 | Btwn 0 v 128} -> _ @-}
    foo = \ x -> body $
           ret $ x `addUint8` x

-- sum :: Def ('[Uint8] :-> Uint8)
-- sum  = proc "sum" $ \ n -> body $ do
--   ifte_ (n >? 1)
--     (do n' <- call sum (n - 1)
--         ret (n' `addUint8` n)
--     )
--     (do ret n
--     )

-- sum :: Def ('[Uint8] :-> Uint8)
-- sum = proc "sum" $ \ n -> body $ do
  
--   let mysum i acc
--         | i == n    = acc
--         | otherwise = mysum (i `addUint8` 1) (acc `addUint8` i)
--   ret (mysum 0 0)
  -- s <- local (ival 0)
  -- i <- local (ival 0)
  -- n `times` \ _ -> do
  --   s' <- deref s
  --   i' <- deref i
  --   store s (s' `addUint8` i')
  --   store i (i' `addUint8` 1)

  -- result <- deref s
  -- ret result
