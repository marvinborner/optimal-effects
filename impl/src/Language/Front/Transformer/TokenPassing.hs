-- Parts were originally written for lambdascope in `Resolver.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Language.Front.Transformer.TokenPassing
  ( transformTokenPassing
  ) where

import           Control.Monad
import           Data.Front
import           Data.List                      ( find )
import qualified Data.Text                     as T
import           Data.TokenPassing
import           GraphRewriting.Graph
import           GraphRewriting.Graph.Write
import           GraphRewriting.Layout.Wrapper as Layout
import           GraphRewriting.Pattern         ( edge )
import           GraphRewriting.Rule
import           Language.TokenPassing.Effects

import           Debug.Trace

type Compiler = Rewrite NodeLS

type Environment = [Name]
data Name = Name
  { symbol    :: T.Text
  , reference :: Compiler Edge
  }

type EnvironmentWrapped = [NameWrapped]
data NameWrapped = NameWrapped
  { symbolWrapped    :: T.Text
  , referenceWrapped :: Replace (Layout.Wrapper NodeLS) Edge
  }

transformTokenPassing :: Term -> Either String (Graph NodeLS)
transformTokenPassing term = Right $ flip execGraph emptyGraph $ do
  o1 <- newEdge
  i  <- newNode Initiator { out = o1 }
  o2 <- newEdge
  t  <- newNode Token { inp = o2, out = o1 }
  compile [] o2 term

toChurch :: Int -> Term
toChurch = Abs "s" . Abs "z" . go
 where
  go 0 = Var "z"
  go n = App (Var "s") (go (n - 1))

-- | Unwrap closure via App-Abs
unwrapClosure :: [(Identifier, Term)] -> Term -> Term
unwrapClosure []             term = term
unwrapClosure ((n, t) : clo) term = App (Abs n (unwrapClosure clo term)) t

-- | This does several def transformations
--   1. desugar params into abstraction: `foo a b c = f` ~> `foo = \a b c.f`
--   2. desugar next-chain of definitions into app-chain of abstractions
--   3. apply itself to every (additionally abstracted) definition using a Rec recursion wrapper
--   4. copy the entire environment upto this point to Rec since they're desugared to effects and can't bind to existing subnets
transformDefs :: Term -> Term
transformDefs = go [] where
  go :: [(Identifier, Term)] -> Term -> Term
  go clo (Def n params body next) = do
    -- TODO: check for closedness, effects can not expand to open terms
    let body' = foldr Abs (unwrapClosure clo body) params
    -- TODO: verify usage of body(')
    let recced = App (Abs n $ go ((n, foldr Abs body params) : clo) next)
                     (Rec n recced $ go clo body')
    recced
  go _ t = t

replaceWrapped
  :: EnvironmentWrapped -> Edge -> Term -> Replace (Layout.Wrapper NodeLS) ()
replaceWrapped env p term =
  case
      trace (show (symbolWrapped <$> env) <> " - " <> show term)
            (transformDefs term)
    of
      App func arg -> do
        f <- byEdge
        x <- byEdge
        byNode $ wrapNodeZero Redirector { portA     = f
                                         , portB     = p
                                         , portC     = x
                                         , direction = BottomRight
                                         }
        replaceWrapped env f func
        replaceWrapped env x arg
      Abs n t -> do
        b         <- byEdge
        (v, name) <- bindNameWrapped n
        void $ byNode $ wrapNodeZero Abstractor { inp = p, body = b, var = v }
        replaceWrapped (name : env) b t
      Rec n rec t -> do
        (v, n') <- bindNameWrapped n
        void $ byNode Effectful
          { inp      = v
          , name     = n
          , function = \out arg -> do
                         active <- byEdge
                         replaceWrapped [] active rec
                         byNode $ wrapNodeZero Redirector { portA     = active
                                                          , portB     = out
                                                          , portC     = arg
                                                          , direction = Top
                                                          }
          }
        replaceWrapped (n' : env) p t
      Eff n -> void $ byNode $ wrapNodeZero $ Effectful
        { inp      = p
        , name     = n
        , function = resolveEffect n
        }
      Var name -> case find (\n -> name == symbolWrapped n) env of
        Just n  -> byWire p =<< referenceWrapped n
        -- Nothing -> void $ newNode $ Effectful { inp = p, name = name, function = ??? }
        Nothing -> error $ "invalid var " <> T.unpack name
      Num n -> replaceWrapped env p $ toChurch n
      If clause true false ->
        replaceWrapped env p $ App (App clause true) false

compile :: Environment -> Edge -> Term -> Compiler ()
compile env p term =
  case
      trace (show (symbol <$> env) <> " - " <> show term) (transformDefs term)
    of
      App func arg -> do
        f <- newEdge
        x <- newEdge
        newNode Redirector { portA     = f
                           , portB     = p
                           , portC     = x
                           , direction = BottomRight
                           }
        compile env f func
        compile env x arg
      Abs n t -> do
        b         <- newEdge
        (v, name) <- bindName n
        void $ newNode Abstractor { inp = p, body = b, var = v }
        compile (name : env) b t
      Rec n rec t -> do
        (v, n') <- bindName n
        void $ newNode Effectful
          { inp      = v
          , name     = n
          , function = \out arg -> do
                         active <- byEdge
                         replaceWrapped [] active rec
                         byNode $ wrapNodeZero Redirector { portA     = active
                                                          , portB     = out
                                                          , portC     = arg
                                                          , direction = Top
                                                          }
          }
        compile (n' : env) p t
      Eff n -> void $ newNode $ Effectful { inp      = p
                                          , name     = n
                                          , function = resolveEffect n
                                          }
      Var name -> case find (\n -> name == symbol n) env of
        Just n  -> mergeEdges p =<< reference n
        -- Nothing -> void $ newNode $ Effectful { inp = p, name = name, function = ??? }
        Nothing -> error $ "invalid var " <> T.unpack name
      Num n                -> compile env p $ toChurch n
      If clause true false -> compile env p $ App (App clause true) false

bindNameWrapped
  :: T.Text -> Replace (Layout.Wrapper NodeLS) (Edge, NameWrapped)
bindNameWrapped sym = do
  v <- byEdge
  let sn = wrapNodeZero Multiplexer { out = v, ins = [] }
  s <- byNode sn
  let ref = do
        e <- byEdge
        -- modifyNode s
        --   $ \s -> wrapNodeZero ((wrappee s) { ins = e : ins (wrappee s) })
        byNode $ wrapNodeZero $ (wrappee sn) { ins = e : ins (wrappee sn) }
        return e
  return (v, NameWrapped { symbolWrapped = sym, referenceWrapped = ref })

bindName :: T.Text -> Compiler (Edge, Name)
bindName sym = do
  v <- newEdge
  s <- newNode Multiplexer { out = v, ins = [] }
  let ref = do
        e <- newEdge
        modifyNode s $ \s -> s { ins = e : ins s }
        return e
  return (v, Name { symbol = sym, reference = ref })
