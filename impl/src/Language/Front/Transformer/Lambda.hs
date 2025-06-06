-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Front.Transformer.Lambda
  ( transformLambda
  ) where


import           Control.Monad.Except
import           Control.Monad.State
import           Data.Effects
import           Data.Front
import qualified Data.Lambda                   as L
import           Data.List                      ( elemIndex )
import qualified Data.Text                     as T

import           Debug.Trace

data Context = Context
  { stk :: [Identifier]
  , clo :: [L.Term]
  }

-- | Transformation monad
type TransM = ExceptT String (State Context)

-- | True if n is used recursively in body
isRecursive :: Identifier -> Term -> Bool
isRecursive n = \case
  Def n' params body next
    | n `elem` params -> isRecursive n next
    | n == n'         -> isRecursive n body
    | otherwise       -> isRecursive n body || isRecursive n next
  Abs n' t | n == n'   -> False
           | otherwise -> isRecursive n t
  If clause true false ->
    isRecursive n clause || isRecursive n true || isRecursive n false
  Var n' | n == n'   -> True
         | otherwise -> False
  App a b     -> isRecursive n a || isRecursive n b
  UnitV       -> False
  Num _       -> False
  Act _ _     -> False
  Do (Unit t) -> isRecursive n t
  Do (Bind n' t a) | n == n'   -> isRecursive n t
                   | otherwise -> isRecursive n t || isRecursive n (Do a)
  e -> error $ show e

-- | Wraps term in n abstractions
wrap :: Int -> L.Term -> L.Term
wrap 0 t = t
wrap n t = L.lam $ wrap (n - 1) t

unwrapClosure :: [L.Term] -> L.Term -> L.Term
unwrapClosure []      term = term
unwrapClosure (t : c) term = L.app (L.lam (unwrapClosure c term)) t

transform :: Term -> TransM L.Term
transform = \case
  Def n params body next
    | isRecursive n body -> do
      ctx@(Context { stk = s, clo = c }) <- get
      put $ ctx { stk = reverse (n : params) ++ s }
      b <- transform body
      let wrapped = wrap (length params) b -- +1 by L.rec
      let rec     = L.rec wrapped (unwrapClosure c rec)
      put $ ctx { stk = n : s, clo = rec : c }
      d <- transform next
      put $ ctx { stk = s, clo = c }
      return $ L.app (L.lam d) rec
    | otherwise -> do
      ctx@(Context { stk = s, clo = c }) <- get
      put $ ctx { stk = reverse params ++ s }
      b <- transform body
      let wrapped = wrap (length params) b
      put $ ctx { stk = n : s, clo = wrapped : c }
      d <- transform next
      put $ ctx { stk = s, clo = c }
      return $ L.app (L.lam d) wrapped
  If clause true false -> do -- TODO: maybe we should also have a parallelIf? or would that just be fork
    clause' <- transform clause
    true'   <- transform (Abs "_" true)
    false'  <- transform (Abs "_" false)
    return $ L.app (L.app (L.app clause' true') false') (L.dat UnitData)
  Var n -> do
    (Context { stk = s }) <- get
    let maybeIdx = elemIndex n s
    case maybeIdx of
      Nothing  -> throwError $ "Identifier not found: " <> T.unpack n
      Just idx -> return $ L.idx idx
  Abs n t -> do
    ctx@(Context { stk = s }) <- get
    put $ ctx { stk = n : s }
    t' <- transform t
    put $ ctx { stk = s }
    return $ L.lam t'
  App a b -> do
    a' <- transform a
    b' <- transform b
    return $ L.app a' b'
  Num n   -> return $ L.dat $ NumberData n
  UnitV   -> return $ L.dat UnitData
  Act a n -> return $ L.act a n
  t       -> error $ show t

transformLambda :: Term -> Either String L.Term
transformLambda t =
  let t' = transform t
  in  evalState (runExceptT t') (Context { stk = [], clo = [] })
