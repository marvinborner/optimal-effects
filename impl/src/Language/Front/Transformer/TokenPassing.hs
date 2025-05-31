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

data ApplicationStrategy = DoPure | DoStrict
  deriving Eq

transformTokenPassing :: Term -> Either String (Graph NodeLS)
transformTokenPassing term = Right $ flip execGraph emptyGraph $ do
  o1 <- newEdge
  i  <- newNode Initiator { out = o1 }
  o2 <- newEdge
  t  <- newNode Token { inp = o2, out = o1 }
  compile [] DoStrict o2 term

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
    -- let body'  = foldr Abs body params
    -- let body'' = go clo body'
    -- let recced = App (Abs n $ go ((n, body'') : clo) next) -- TODO: there may be a problem with body'' when it contains recursions?
    --                  (Rec n (unwrapClosure clo recced) body'')
    -- recced
    -- TODO: recursion

    let body' = foldr Abs body params
    App (Abs n $ go clo next) body'
  go _ t = t

-- TODO: somehow get rid of this awful code duplication
replaceWrapped
  :: EnvironmentWrapped
  -> ApplicationStrategy
  -> Edge
  -> Term
  -> Replace (Layout.Wrapper NodeLS) ()
replaceWrapped env strategy p term =
  case
      trace
        (show (symbolWrapped <$> env) <> " -w " <> show (transformDefs term))
        (transformDefs term)
    of
      App func arg -> do
        f <- byEdge
        x <- byEdge
        byNode $ wrapNodeZero Redirector
          { portA     = f
          , portB     = p
          , portC     = x
          , direction = if strategy == DoPure then Top else BottomLeft
          }
        replaceWrapped env strategy f func
        replaceWrapped env strategy x arg
      Abs n t -> do
        b         <- byEdge
        (v, name) <- bindNameWrapped n
        void $ byNode $ wrapNodeZero Abstractor { inp = p, body = b, var = v }
        replaceWrapped (name : env) strategy b t
      -- Rec n rec t -> do
      --   (v, name) <- bindNameWrapped n
      --   void $ byNode Effectful
      --     { inp      = v
      --     , name     = n
      --     , function = \out arg -> do
      --                    active <- byEdge
      --                    replaceWrapped [] strategy active rec
      --                    byNode $ wrapNodeZero Redirector { portA     = active
      --                                                     , portB     = out
      --                                                     , portC     = arg
      --                                                     , direction = Top
      --                                                     }
      --     }
      --   replaceWrapped (name : env) strategy p t
      Eff a n -> void $ byNode $ wrapNodeZero $ Effectful
        { inp      = p
        , cur      = p
        , name     = n
        , arity    = a
        , function = resolveEffect n
        , args     = []
        }
      Var name -> case find (\n -> name == symbolWrapped n) env of
        Just n  -> byWire p =<< referenceWrapped n
        -- Just n  -> do
        --   r <- referenceWrapped n
        --   replace $ void $ mergeEdges p r
        -- Nothing -> void $ newNode $ Effectful { inp = p, name = name, function = ??? }
        Nothing -> error $ "invalid var " <> T.unpack name
      Num n ->
        void $ byNode $ wrapNodeZero $ Data { inp = p, dat = NumberData n }
      UnitV -> void $ byNode $ wrapNodeZero $ Data { inp = p, dat = UnitData }
      If clause true false ->
        replaceWrapped env strategy p $ App (App clause true) false
      Pure   t -> replaceWrapped env DoPure p t
      Strict t -> replaceWrapped env DoStrict p t

compile :: Environment -> ApplicationStrategy -> Edge -> Term -> Compiler ()
compile env strategy p term =
  case
      trace (show (symbol <$> env) <> " -c " <> show (transformDefs term))
            (transformDefs term)
    of
      App func arg -> do
        f <- newEdge
        x <- newEdge
        newNode Redirector
          { portA     = f
          , portB     = p
          , portC     = x
          , direction = if strategy == DoPure then Top else BottomLeft
          }
        compile env strategy f func
        compile env strategy x arg
      Abs n t -> do
        b         <- newEdge
        (v, name) <- bindName n
        void $ newNode Abstractor { inp = p, body = b, var = v }
        compile (name : env) strategy b t
      -- Rec n rec t -> do
      --   (v, name) <- bindName n
      --   void $ newNode Effectful
      --     { inp      = v
      --     , name     = n
      --     , function = \out arg -> do
      --                    active <- byEdge
      --                    replaceWrapped [] strategy active rec
      --                    byNode $ wrapNodeZero Redirector { portA     = active
      --                                                     , portB     = out
      --                                                     , portC     = arg
      --                                                     , direction = Top
      --                                                     }
      --     }
      --   compile (name : env) strategy p t
      Eff a n -> void $ newNode $ Effectful { inp      = p
                                            , cur      = p
                                            , name     = n
                                            , arity    = a
                                            , function = resolveEffect n
                                            , args     = []
                                            }
      Var name -> case find (\n -> name == symbol n) env of
        Just n  -> mergeEdges p =<< reference n
        -- Nothing -> void $ newNode $ Effectful { inp = p, name = name, function = ??? }
        Nothing -> error $ "invalid var " <> T.unpack name
      Num n -> void $ newNode $ Data { inp = p, dat = NumberData n }
      UnitV -> void $ newNode $ Data { inp = p, dat = UnitData }
      If clause true false ->
        compile env strategy p $ App (App clause true) false
      Pure   t -> compile env DoPure p t
      Strict t -> compile env DoStrict p t

bindNameWrapped
  :: T.Text -> Replace (Layout.Wrapper NodeLS) (Edge, NameWrapped)
bindNameWrapped sym = do
  v <- byEdge
  let sn = wrapNodeZero Multiplexer { out = v, ins = [] }
  s <- byNode sn
  let ref = do
        e <- byEdge
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
