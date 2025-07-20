-- Copyright (c) 2025, Marvin Borner
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, FlexibleContexts #-}

module Language.Generic.Effects
  ( executeActor
  ) where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Data.Effects                   ( EffectData(..)
                                                , EffectFunction
                                                )
import qualified Data.Text                     as T
import           Data.View
import           GraphRewriting.Graph.Types
import           GraphRewriting.Rule
import           Language.Generic.Node
import           System.IO.Unsafe               ( unsafePerformIO ) -- safe
import           System.Process                 ( readProcess )


import           Debug.Trace

churchTrue
  :: forall m n . (GenericNode m, View [Port] n, View m n) => Edge -> Rule n
churchTrue p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ gEraser @m era
  byNode $ gAbstractor @m tok con var
  byNode $ gAbstractor @m con var era
  byNode $ gToken @m p tok

churchFalse
  :: forall m n . (GenericNode m, View [Port] n, View m n) => Edge -> Rule n
churchFalse p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ gEraser @m era
  byNode $ gAbstractor @m tok con era
  byNode $ gAbstractor @m con var var
  byNode $ gToken @m p tok

evalProcess :: String -> [String] -> IO (Either String Int)
evalProcess proc args = do
  result <- try (readProcess proc args "") :: IO (Either SomeException String)
  case result of
    Left  err    -> return $ Left $ "execution failed: " <> show err
    Right output -> case reads (filter (/= '\n') output) :: [(Int, String)] of
      [(n, "")] -> return $ Right n
      res       -> return $ Left $ "output not an integer: " <> show res

evalLang :: String -> String -> Int -> IO (Either String Int)
evalLang "python" func arg = evalProcess
  "python3"
  ["-c", "import math; print(" <> func <> "(" <> show arg <> "))"]
evalLang "julia" func arg =
  evalProcess "julia" ["-e", "println(" <> func <> "(" <> show arg <> "))"]
evalLang "r" func arg =
  evalProcess "Rscript" ["-e", "cat(" <> func <> "(" <> show arg <> "))"]

-- TODO: allow IO via monad
executeActor
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => T.Text
  -> [EffectData]
  -> Port
  -> Rule n
executeActor "readInt" [UnitData] p = replace $ do
  tok <- byEdge -- send token back!
  trace "readInt" $ byNode $ gData @m tok (NumberData 42)
  byNode $ gToken @m p tok

executeActor "writeInt" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("writeInt: " <> show n) $ byNode $ gData @m tok UnitData
  byNode $ gToken @m p tok

executeActor "print" [StringData s] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("print: " <> show s) $ byNode $ gData @m tok UnitData
  byNode $ gToken @m p tok

executeActor "readFile" [StringData f] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("readFile: " <> show f) $ byNode $ gData @m
    tok
    (StringData $ "content of " <> f)
  byNode $ gToken @m p tok

executeActor "writeFile" [StringData c, StringData f] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("writeFile: " <> show f <> " <- " <> show c) $ byNode $ gData @m
    tok
    UnitData
  byNode $ gToken @m p tok

executeActor "succ" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("succ: " <> show n) $ byNode $ gData @m tok (NumberData $ n + 1)
  byNode $ gToken @m p tok

executeActor "pred" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("pred: " <> show n) $ byNode $ gData @m tok (NumberData $ n - 1)
  byNode $ gToken @m p tok

executeActor "equal" [NumberData b, NumberData a] p
  | a == b = trace ("equal: " <> show a <> " " <> show b) $ churchTrue @m p
  | a /= b = trace ("not equal: " <> show a <> " " <> show b) $ churchFalse @m p

executeActor "add" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("add: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a + b)
  byNode $ gToken @m p tok

executeActor "sub" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("sub: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a - b)
  byNode $ gToken @m p tok

executeActor "mul" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("mul: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a * b)
  byNode $ gToken @m p tok

executeActor "div" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("div: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (NumberData $ a `div` b)
  byNode $ gToken @m p tok

executeActor "download" [StringData url, NumberData 0] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("download: " <> show url) $ byNode $ gData @m
    tok
    (StringData $ "content of " <> url)
  byNode $ gToken @m p tok

executeActor "download" [StringData url, NumberData i] p = replace $ do
  tok <- byEdge -- bounce token!!
  byNode $ gActor @m tok "download" 0 [StringData url, NumberData $ i - 1]
  byNode $ gToken @m tok p

executeActor "evalLang" [NumberData arg, StringData func, StringData lang] p =
  replace $ do
    let result = unsafePerformIO $ evalLang lang func arg
    trace ("evalLang: " <> show lang <> " " <> show func <> " " <> show arg)
          (return ())
    case result of
      Left err ->
        trace (lang <> " failed: " <> err) (byNode $ gData @m p UnitData)
      Right ok -> do
        tok <- byEdge -- send token back!
        byNode $ gData @m tok (NumberData ok)
        byNode $ gToken @m p tok

executeActor "concat" [StringData b, StringData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("concat: " <> show a <> " " <> show b) $ byNode $ gData @m
    tok
    (StringData $ a <> b)
  byNode $ gToken @m p tok

executeActor n args _ =
  error $ "invalid action " <> show n <> ": " <> show args
