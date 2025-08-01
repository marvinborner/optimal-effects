-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Front.Transformer.Lambda
  ( transformLambda
  ) where


import           Control.Monad.Except
import           Control.Monad.State
import           Data.Effects
import           Data.Fix
import           Data.Front
import qualified Data.Lambda                   as L
import           Data.List                      ( elemIndex )
import qualified Data.Text                     as T

import           Debug.Trace

data Context = Context
  { stk :: [Identifier]
  , clo :: [(L.Term, Bool)]
  }

-- | Transformation monad
type TransM = ExceptT String (State Context)

-- | True if n is used recursively in body
-- TODO: this should be done with a simple fv check
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
  App a b -> isRecursive n a || isRecursive n b
  UnitV   -> False
  Num _   -> False
  Str _   -> False
  Act _ _ -> False
  Token   -> False
  Idx  _  -> False -- hmm
  Unit t  -> isRecursive n t
  Bind n' t a | n == n'   -> isRecursive n t
              | otherwise -> isRecursive n t || isRecursive n (Do a)
  Do t       -> isRecursive n t
  Fork _ a b -> isRecursive n a || isRecursive n b
  e          -> error $ show e

-- | Wraps term in n abstractions
wrap :: Int -> L.Term -> L.Term
wrap 0 t = t
wrap n t = L.lam $ wrap (n - 1) t

-- | Unwrap closure on a term via applications
-- | The boolean specifies whether a term "is a definition" which receives arguments
-- | Future work should decode non-definitions to global references TODO
unwrapClosure :: [(L.Term, Bool)] -> L.Term -> L.Term
unwrapClosure []              term = term
unwrapClosure ((t, True) : c) term = L.app (L.lam $ unwrapClosure c term) t
unwrapClosure ((t, False) : c) term =
  L.app (L.lam $ unwrapClosure c term) (L.dat UnitData)
  -- L.app (L.lam $ unwrapClosure c term) (L.lam (shift 1 t))

shift :: Int -> L.Term -> L.Term
shift n = go 0
 where
  go d = L.para $ \case
    L.Idx i | i >= d    -> L.idx $ i + n
            | otherwise -> L.idx $ i
    L.Lam (t, _) -> L.lam $ go (d + 1) t
    t            -> Fix $ fmap snd t

transform :: Term -> TransM L.Term
transform = \case
  Def n params body next
    | isRecursive n body -> do
      ctx@(Context { stk = s, clo = c }) <- get
      put $ ctx { stk = reverse (n : params) ++ s }
      b <- transform body
      let wrapped      = wrap (length params) b -- +1 by L.rec
      let rec          = L.rec wrapped $ unwrapClosure (reverse c) rec
      let isDefinition = not $ null params
      put $ ctx { stk = n : s, clo = (rec, isDefinition) : c }
      d <- transform next
      put $ ctx { stk = s, clo = c }
      return $ L.app (L.lam d) rec
    | otherwise -> do
      ctx@(Context { stk = s, clo = c }) <- get
      put $ ctx { stk = reverse params ++ s }
      b <- transform body
      let wrapped      = wrap (length params) b
      let isDefinition = not $ null params
      put $ ctx { stk = n : s, clo = (wrapped, isDefinition) : c }
      d <- transform next
      put $ ctx { stk = s, clo = c }
      return $ L.app (L.lam d) wrapped
  If clause true false -> do
    clause' <- transform clause
    true'   <- transform true
    false'  <- transform false
    return $ L.app (L.app clause' true') false'
  Var n -> do
    (Context { stk = s }) <- get
    let maybeIdx = elemIndex n s
    case maybeIdx of
      Nothing ->
        throwError
          $  "Identifier not found: "
          <> T.unpack n
          <> ", stack: "
          <> show s
      Just idx -> return $ L.idx idx
  Idx n   -> return $ L.idx n -- TODO: verify closedness in stack
  Abs n t -> do -- these do not support recursion (typically anonymous)
    ctx@(Context { stk = s }) <- get
    put $ ctx { stk = n : s }
    t' <- transform t
    put $ ctx { stk = s }
    return $ L.lam t'
  App a b -> do
    a' <- transform a
    b' <- transform b
    return $ L.app a' b'
  Num n                -> return $ L.dat $ NumberData n
  Str s                -> return $ L.dat $ StringData s
  UnitV                -> return $ L.dat UnitData
  Act a n              -> return $ L.act a n
  Token                -> return L.tok

  Fork Conjunctive a b -> L.frk L.Conjunctive <$> transform a <*> transform b
  Fork Disjunctive a b -> L.frk L.Disjunctive <$> transform a <*> transform b

  -- TODO: rec closure?
  Bind v           t n -> do
    ctx@(Context { stk = s }) <- get
    t'                        <- transform t
    put $ ctx { stk = v : s }
    n' <- transform (Do n)
    put $ ctx { stk = s }
    return $ L.bnd t' (L.lam n')
  Unit t -> do
    t' <- transform t
    return $ L.eta t'
  Do t -> transform t

  t    -> error $ show t

transformLambda :: Term -> Either String L.Term
transformLambda t =
  let t' = transform t
  in  evalState (runExceptT t') (Context { stk = [], clo = [] })
