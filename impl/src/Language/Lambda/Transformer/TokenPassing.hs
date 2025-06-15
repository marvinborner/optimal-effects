-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE FlexibleContexts #-}

module Language.Lambda.Transformer.TokenPassing
  ( transformTokenPassing
  , executeRecursor
  ) where

import           Control.Monad.State
import           Data.Lambda                    ( para )
import qualified Data.Lambda                   as L
import           Data.TokenPassing
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

transformTokenPassing :: L.Term -> Either String (Graph NodeTP)
transformTokenPassing term = do
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
-- TODO: translate token by app match
compile
  :: Monad m
  => (NodeTP -> m a1)
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
  L.App (_, f) (_, a) -> do
    ctx1@(Context { port = pf }) <- f
    ctx2@(Context { port = pa }) <- a
    o                            <- edge
    node Redirector { portA     = pf
                    , portB     = o
                    , portC     = pa
                    , direction = BottomLeft
                    }
    return $ (ctx1 <> ctx2) { port = o }
  L.Idx i -> do
    o <- edge
    return $ Context { port = o, bindings = [(i, o)] }
  L.Rec (_, f) (rec, _) -> do
    -- since f must always be an abstraction, we let the application to rec interact immediately
    -- this way, we don't end in endless recursion when the rec always gets unwrapped by the token
    ctx@(Context { bindings = bs, port = p }) <- f
    x <- bindName node edge conn ctx
    r <- edge -- rec
    node Recursor { inp = r, boxed = rec }
    conn x r -- bind x to rec
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = p }
  L.Act n a -> do
    o <- edge
    node $ Actor { inp = o, name = n, arity = a, args = [] }
    return $ Context { port = o, bindings = [] }
  L.Dat d -> do
    o <- edge
    node $ Data { inp = o, dat = d }
    return $ Context { port = o, bindings = [] }
  _ -> error ""

-- | create multiplexer of all =0 bindings
-- TODO: we could additionally return a new context with the popped bindings
bindName :: Monad m => (NodeTP -> m a) -> m Port -> p -> Context -> m Port
bindName node edge conn (Context { bindings = bs }) = do
  x <- edge
  let bound = snd <$> filter (\(i, _) -> i == 0) bs
  node Multiplexer { out = x, ins = bound }
  return x

executeRecursor :: (View [Port] n, View NodeTP n) => L.Term -> Port -> Rule n
executeRecursor boxed o = replace $ do
  tok                    <- byEdge
  (Context { port = p }) <- compile byNode byEdge byWire boxed
  byWire p tok
  byNode Token { inp = tok, out = o }
