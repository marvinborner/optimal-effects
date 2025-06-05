-- Copyright (c) 2025, Marvin Borner

module Language.Lambda.Transformer.TokenPassing
  ( transformTokenPassing
  ) where

import           Control.Monad.State
import           Data.Lambda                    ( para )
import qualified Data.Lambda                   as L
import           Data.TokenPassing
import           GraphRewriting.Graph
import           GraphRewriting.Graph.Write
import           GraphRewriting.Pattern         ( edge )
import           GraphRewriting.Rule
import           Language.TokenPassing.Effects

import           GraphRewriting.Layout.Wrapper as Layout

data Context = Context
  { bindings :: [(Int, Edge)]
  , port     :: Edge
  }
type Compiler = Rewrite NodeTP

instance Semigroup Context where
  ctx@(Context { bindings = bs1 }) <> (Context { bindings = bs2 }) =
    ctx { bindings = bs1 <> bs2 }

transformTokenPassing :: L.Term -> Either String (Graph NodeTP)
transformTokenPassing term = do
  let (bindings, graph) = flip runGraph emptyGraph $ do
        context@(Context { port = n, bindings = bs }) <- compile term
        o1 <- newEdge
        i  <- newNode Initiator { out = o1 }
        o2 <- newEdge
        t  <- newNode Token { inp = o2, out = o1 }
        mergeEdges o2 n
        return bs
  when (any (\(x, _) -> x >= 0) bindings)
       (Left $ "term is open " <> show bindings)
  return graph

replaceWrapped :: L.Term -> Replace (Layout.Wrapper NodeTP) Context
replaceWrapped = para $ \case
  L.Lam (_, t) -> do
    ctx@(Context { bindings = bs, port = p }) <- t
    o <- byEdge
    v <- bindNameWrapped ctx
    byNode $ wrapNodeZero Abstractor { inp = o, body = p, var = v }
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = o }
  L.App (_, f) (_, a) -> do
    ctx1@(Context { port = pf }) <- f
    ctx2@(Context { port = pa }) <- a
    o                            <- byEdge
    byNode $ wrapNodeZero Redirector { portA     = pf
                                     , portB     = o
                                     , portC     = pa
                                     , direction = BottomLeft
                                     }
    return $ (ctx1 <> ctx2) { port = o }
  L.Idx i -> do
    o <- byEdge
    return $ Context { port = o, bindings = [(i, o)] }
  L.Rec (_, f) (rec, _) -> do
    -- since f must always be an abstraction, we let the application to rec interact immediately
    -- this way, we don't end in endless recursion when the rec always gets unwrapped by the token
    ctx@(Context { bindings = bs, port = p }) <- f
    v <- bindNameWrapped ctx
    r <- byEdge -- rec
    byNode $ wrapNodeZero Actor
      { inp      = r
      , cur      = r
      , name     = "rec"
      , arity    = 0
      , function = \_ o2 -> do
                     tok                    <- byEdge -- bounce token
                     (Context { port = p }) <- replaceWrapped rec
                     byWire p tok
                     byNode $ wrapNodeZero Token { inp = tok, out = o2 }
      }
    byWire v r -- bind x to rec
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = p }
  L.Act n a -> do
    o <- byEdge
    byNode $ wrapNodeZero Actor { inp      = o
                                , cur      = o
                                , name     = n
                                , arity    = a
                                , function = resolveEffect n
                                , args     = []
                                }
    return $ Context { port = o, bindings = [] }
  L.Dat d -> do
    o <- byEdge
    byNode $ wrapNodeZero Data { inp = o, dat = d }
    return $ Context { port = o, bindings = [] }
  _ -> error ""

-- we use a para instead cata to expand the original term within rec
-- TODO: translate token by app match
compile :: L.Term -> Compiler Context
compile = para $ \case
  L.Lam (_, t) -> do
    ctx@(Context { bindings = bs, port = p }) <- t
    o <- newEdge
    v <- bindName ctx
    newNode Abstractor { inp = o, body = p, var = v }
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = o }
  L.App (_, f) (_, a) -> do
    ctx1@(Context { port = pf }) <- f
    ctx2@(Context { port = pa }) <- a
    o                            <- newEdge
    newNode Redirector { portA     = pf
                       , portB     = o
                       , portC     = pa
                       , direction = BottomLeft
                       }
    return $ (ctx1 <> ctx2) { port = o }
  L.Idx i -> do
    o <- newEdge
    return $ Context { port = o, bindings = [(i, o)] }
  L.Rec (_, f) (rec, _) -> do
    -- since f must always be an abstraction, we let the application to rec interact immediately
    -- this way, we don't end in endless recursion when the rec always gets unwrapped by the token
    ctx@(Context { bindings = bs, port = p }) <- f
    v <- bindName ctx
    r <- newEdge -- rec
    newNode Actor
      { inp      = r
      , cur      = r
      , name     = "rec"
      , arity    = 0
      , function = \_ o2 -> do
                     tok                    <- byEdge -- bounce token
                     (Context { port = p }) <- replaceWrapped rec
                     byWire p tok
                     byNode $ wrapNodeZero Token { inp = tok, out = o2 }
      }
    mergeEdges v r -- bind x to rec
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = p }
  L.Act n a -> do
    o <- newEdge
    newNode $ Actor { inp      = o
                    , cur      = o
                    , name     = n
                    , arity    = a
                    , function = resolveEffect n
                    , args     = []
                    }
    return $ Context { port = o, bindings = [] }
  L.Dat d -> do
    o <- newEdge
    newNode $ Data { inp = o, dat = d }
    return $ Context { port = o, bindings = [] }
  _ -> error ""

bindNameWrapped :: Context -> Replace (Layout.Wrapper NodeTP) Edge
bindNameWrapped (Context { bindings = bs }) = do
  v <- byEdge
  let bound = snd <$> filter (\(i, _) -> i == 0) bs
  byNode $ wrapNodeZero Multiplexer { out = v, ins = bound }
  return v

-- | create multiplexer of all =0 bindings
-- TODO: we could additionally return a new context with the popped bindings
bindName :: Context -> Compiler Edge
bindName (Context { bindings = bs }) = do
  v <- newEdge
  let bound = snd <$> filter (\(i, _) -> i == 0) bs
  newNode Multiplexer { out = v, ins = bound }
  return v
