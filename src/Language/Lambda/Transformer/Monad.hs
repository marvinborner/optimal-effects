-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE FlexibleContexts #-}

module Language.Lambda.Transformer.Monad
  ( transformMonad
  , executeRecursor
  ) where

import           Control.Monad.State
import           Data.Fix
import           Data.Lambda                    ( para )
import qualified Data.Lambda                   as L
import           Data.Monad
import           GraphRewriting.Graph
import           GraphRewriting.Graph.Write
import           GraphRewriting.Pattern
import           GraphRewriting.Rule
import           Language.Generic.Effects

import           Data.Coerce

data Context = Context
  { bindings :: [(Int, Edge)]
  , port     :: Edge
  }

instance Semigroup Context where
  ctx@(Context { bindings = bs1 }) <> (Context { bindings = bs2 }) =
    ctx { bindings = bs1 <> bs2 }

transformMonad :: L.Term -> Either String (Graph NodeMS)
transformMonad term = do
  let (bindings, graph) = flip runGraph emptyGraph $ do
        context@(Context { port = n, bindings = bs }) <- compile newNode
                                                                 newEdge
                                                                 mergeEdges
                                                                 term
        o1 <- newEdge
        i  <- newNode Initiator { out = o1 }
        o2 <- newEdge
        t  <- newNode Token { inp = o2, out = o1 }
        mergeEdges o2 n
        return bs
  when (any (\(x, _) -> x >= 0) bindings)
       (Left $ "term is open " <> show bindings)
  return graph

-- we use a para instead cata to expand the original term within rec
compile
  :: Monad m
  => (NodeMS -> m a1)
  -> m Port
  -> (Port -> Port -> m a2)
  -> L.Term
  -> m Context
compile node edge conn = para $ \case
  L.Lam (_, t) -> do
    ctx@(Context { bindings = bs, port = p }) <- t
    o <- edge
    x <- bindName node edge conn ctx
    node Abstractor { inp = o, body = p, var = x }
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = o }
  L.App (_, t) (Fix L.Tok, _) -> do -- (f a !) async actions
    ctx@(Context { port = p }) <- t
    o1                         <- edge
    o2                         <- edge
    -- we send 2 tokens that erase eachother after the first is reflected
    -- this prevents rogue threads spreading from async actions
    node Token { inp = p, out = o1 }
    node Token { inp = o1, out = o2 }
    return $ ctx { port = o2 }
  L.App (_, f) (_, a) -> do
    ctx1@(Context { port = pf }) <- f
    ctx2@(Context { port = pa }) <- a
    o                            <- edge
    node Applicator { inp = o, func = pf, arg = pa }
    return $ (ctx1 <> ctx2) { port = o }
  L.Idx i -> do
    o <- edge
    return $ Context { port = o, bindings = [(i, o)] }
  L.Rec (_, f) (rec, _) -> do
    -- since f must always be an abstraction, we let the application to rec interact immediately
    ctx@(Context { bindings = bs, port = p }) <- f
    x <- bindName node edge conn ctx
    r <- edge -- rec
    node Recursor { inp = r, boxed = rec }
    conn x r -- bind x to rec
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = p }
  L.Act n a -> do
    o <- edge
    node Actor { inp = o, name = n, arity = a, args = [] }
    return Context { port = o, bindings = [] }
  L.Dat d -> do
    o <- edge
    node Data { inp = o, dat = d }
    return Context { port = o, bindings = [] }
  L.Frk tpe (_, lhs) (_, rhs) -> do
    o <- edge
    (Context { bindings = bsL, port = pL }) <- lhs
    (Context { bindings = bsR, port = pR }) <- rhs
    node Fork { tpe = tpe, inp = o, lhs = pL, rhs = pR, exec = False }
    return Context { port = o, bindings = bsL <> bsR }
  L.Bnd (_, t) (_, n) -> do
    (Context { bindings = bsT, port = pT }) <- t
    (Context { bindings = bsN, port = pN }) <- n
    -- shifts are handled by abs (always immediate in n)
    o <- edge
    node BindN { inp = o, arg = pT, var = pN, exec = False }
    return $ Context { port = o, bindings = bsT <> bsN }
  L.Eta (_, t) -> do
    o <- edge
    ctx@(Context { bindings = bs, port = cont }) <- t
    node UnitN { inp = o, out = cont }
    return $ ctx { port = o, bindings = bs }
  _ -> error "invalid term"

-- | create multiplexer of all =0 bindings
-- TODO: we could additionally return a new context with the popped bindings
bindName :: Monad m => (NodeMS -> m a) -> m Port -> p -> Context -> m Port
bindName node edge conn (Context { bindings = bs }) = do
  x <- edge
  let bound = snd <$> filter (\(i, _) -> i == 0) bs
  node Multiplexer { out = x, ins = bound }
  return x

executeRecursor :: (View [Port] n, View NodeMS n) => L.Term -> Port -> Rule n
executeRecursor boxed o = replace $ do
  tok                    <- byEdge
  (Context { port = p }) <- compile byNode byEdge byWire boxed
  byWire p tok
  byNode Token { inp = o, out = tok }
