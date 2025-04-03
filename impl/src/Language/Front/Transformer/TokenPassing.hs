-- Parts were originally written for lambdascope in `Resolver.hs`
--   under the BSD-3-Clause License (as in src/GraphRewriting/LICENSE)
-- Copyright (c) 2010, Jan Rochel
-- Copyright (c) 2024, Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Language.Front.Transformer.TokenPassing
  ( transformTokenPassing
  ) where

import           Control.Monad
import           Data.Front
import qualified Data.Text                     as T
import           Data.TokenPassing
import           GraphRewriting.Graph
import           GraphRewriting.Graph.Write


type Compiler = Rewrite NodeLS

type Environment = [Name]
data Name = Name
  { symbol    :: String
  , reference :: Compiler Edge
  }

transformTokenPassing :: Term -> Either String (Graph NodeLS)
transformTokenPassing term = Right $ flip execGraph emptyGraph $ do
  o <- newEdge
  i <- newNode Initiator { out = o }
  compile [] o term

toChurch :: Int -> Term
toChurch = Abs "s" . Abs "z" . go
 where
  go 0 = Var "z"
  go n = App (Var "s") (go (n - 1))

effectArity :: T.Text -> Int
effectArity "readInt"  = 0
effectArity "writeInt" = 1

compile :: Environment -> Edge -> Term -> Compiler ()
compile env p term = case term of
  App func arg -> do
    f <- newEdge
    x <- newEdge
    -- TODO: compile to rotated application
    void $ newNode Applicator { inp = p, func = f, arg = x }
    compile env f func
    compile env x arg
  Abs x e -> do
    b         <- newEdge
    (v, name) <- bindName (T.unpack x)
    void $ newNode Abstractor { inp = p, body = b, var = v }
    compile (name : env) b e
  Def n params body next -> do
    (e, n') <- bindName (T.unpack n)
    let env' = n' : env
    let t    = foldr Abs body params
    void $ compile env' e t
    compile env' p next
  Eff n    -> void $ newNode $ Effectful { inp = p, name = T.unpack n }
  Var name -> case env of -- TODO: recursion
    []     -> void $ newNode (Effectful { inp = p, name = T.unpack name })
    n : ns -> if T.unpack name == symbol n
      then mergeEdges p =<< reference n
      else compile ns p term
  Num n                -> compile env p (toChurch n)
  If clause true false -> compile env p $ App (App clause true) false

bindName :: String -> Compiler (Edge, Name)
bindName sym = do
  v <- newEdge
  s <- newNode Multiplexer { out = v, ins = [] }
  let ref = do
        e <- newEdge
        modifyNode s $ \s -> s { ins = e : ins s }
        return e
  return (v, Name { symbol = sym, reference = ref })
