-- Copyright (c) 2025, Marvin Borner

module Language.Lambda.Transformer.TokenPassing
  ( transformTokenPassing
  ) where

import           Control.Monad.State
import           Data.Fix
import qualified Data.Lambda                   as L
import           Data.TokenPassing
import           GraphRewriting.Graph
import           GraphRewriting.Graph.Write
import           GraphRewriting.Pattern         ( edge )
import           GraphRewriting.Rule
import           Language.TokenPassing.Effects

-- type Compiler = Rewrite NodeTP

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
  when (any (\(x, _) -> x > 0) bindings)
       (Left $ "term is open " <> show bindings)
  return graph

-- TODO: translate token by app match
compile :: L.Term -> Compiler Context
compile = foldFix $ \case
  L.Lam t -> do
    ctx@(Context { bindings = bs, port = p }) <- t
    o <- newEdge
    v <- bindName ctx
    newNode Abstractor { inp = o, body = p, var = v }
    let bs' = (\(i, e) -> (i - 1, e)) <$> bs
    return $ ctx { bindings = bs', port = o }
  L.App f a -> do
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

-- | create multiplexer of all =0 bindings
-- TODO: we could additionally return a new context with the popped bindings
bindName :: Context -> Compiler Edge
bindName (Context { bindings = bs }) = do
  v <- newEdge
  let bound = snd <$> filter (\(i, _) -> i == 0) bs
  s <- newNode Multiplexer { out = v, ins = bound }
  return v
