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
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> Edge
  -> Rule n
churchTrue w p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ gWrap (gEraser @m era) w
  byNode $ gWrap (gAbstractor @m tok con var) w
  byNode $ gWrap (gAbstractor @m con var era) w
  byNode $ gWrap (gToken @m p tok) w

churchFalse
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> Edge
  -> Rule n
churchFalse w p = replace $ do
  tok <- byEdge -- send token back!
  var <- byEdge
  era <- byEdge
  con <- byEdge
  byNode $ gWrap (gEraser @m era) w
  byNode $ gWrap (gAbstractor @m tok con era) w
  byNode $ gWrap (gAbstractor @m con var var) w
  byNode $ gWrap (gToken @m p tok) w

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
{-# NOINLINE executeActor #-}
executeActor
  :: forall m n
   . (GenericNode m, View [Port] n, View m n)
  => WrapType
  -> T.Text
  -> [EffectData]
  -> Port
  -> Rule n
executeActor w "readInt" [UnitData] p = replace $ do
  tok <- byEdge -- send token back!
  trace "readInt" $ byNode $ gWrap (gData @m tok (NumberData 42)) w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "writeInt" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("writeInt: " <> show n) $ byNode $ gWrap (gData @m tok UnitData) w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "print" [StringData s] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("print: " <> show s) $ byNode $ gWrap (gData @m tok UnitData) w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "readFile" [StringData f] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("readFile: " <> show f) $ byNode $ gWrap
    (gData @m tok (StringData $ "content of " <> f))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "writeFile" [StringData c, StringData f] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("writeFile: " <> show f <> " <- " <> show c) $ byNode $ gWrap
    (gData @m tok UnitData)
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "succ" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("succ: " <> show n) $ byNode $ gWrap
    (gData @m tok (NumberData $ n + 1))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "pred" [NumberData n] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("pred: " <> show n) $ byNode $ gWrap
    (gData @m tok (NumberData $ n - 1))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "isNotEqual" [NumberData b, NumberData a] p
  | a /= b = trace ("not equal: " <> show a <> " " <> show b)
  $ churchTrue @m w p
  | otherwise = trace ("equal: " <> show a <> " " <> show b)
  $ churchFalse @m w p

executeActor w "isEqual" [NumberData b, NumberData a] p
  | a == b = trace ("equal: " <> show a <> " " <> show b) $ churchTrue @m w p
  | otherwise = trace ("not equal: " <> show a <> " " <> show b)
  $ churchFalse @m w p

executeActor w "isLess" [NumberData b, NumberData a] p
  | a < b = trace ("less: " <> show a <> " " <> show b) $ churchTrue @m w p
  | otherwise = trace ("not less: " <> show a <> " " <> show b)
  $ churchFalse @m w p

executeActor w "isGreater" [NumberData b, NumberData a] p
  | a > b
  = trace ("greater: " <> show a <> " " <> show b) $ churchTrue @m w p
  | otherwise
  = trace ("not greater: " <> show a <> " " <> show b) $ churchFalse @m w p

executeActor w "add" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("add: " <> show a <> " " <> show b) $ byNode $ gWrap
    (gData @m tok (NumberData $ a + b))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "sub" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("sub: " <> show a <> " " <> show b) $ byNode $ gWrap
    (gData @m tok (NumberData $ a - b))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "mul" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("mul: " <> show a <> " " <> show b) $ byNode $ gWrap
    (gData @m tok (NumberData $ a * b))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "div" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("div: " <> show a <> " " <> show b) $ byNode $ gWrap
    (gData @m tok (NumberData $ a `div` b))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "mod" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("mod: " <> show a <> " " <> show b) $ byNode $ gWrap
    (gData @m tok (NumberData $ a `mod` b))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "pow" [NumberData b, NumberData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("pow: " <> show a <> " " <> show b) $ byNode $ gWrap
    (gData @m tok (NumberData $ a ^ b))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "download" [StringData url, NumberData 0] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("download: " <> show url) $ byNode $ gWrap
    (gData @m tok (StringData $ "content of " <> url))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w "download" [StringData url, NumberData i] p = replace $ do
  tok <- byEdge -- bounce token!!
  byNode $ gWrap
    (gActor @m tok "download" 0 [StringData url, NumberData $ i - 1])
    w
  byNode $ gWrap (gToken @m tok p) w

executeActor w "evalLang" [NumberData arg, StringData func, StringData lang] p
  = replace $ do
    let result = unsafePerformIO $ evalLang lang func arg
    trace ("evalLang: " <> show lang <> " " <> show func <> " " <> show arg)
          (return ())
    case result of
      Left err -> trace (lang <> " failed: " <> err)
                        (byNode $ gWrap (gData @m p UnitData) w)
      Right ok -> do
        tok <- byEdge -- send token back!
        byNode $ gWrap (gData @m tok (NumberData ok)) w
        byNode $ gWrap (gToken @m p tok) w

executeActor w "concat" [StringData b, StringData a] p = replace $ do
  tok <- byEdge -- send token back!
  trace ("concat: " <> show a <> " " <> show b) $ byNode $ gWrap
    (gData @m tok (StringData $ a <> b))
    w
  byNode $ gWrap (gToken @m p tok) w

executeActor w n args _ =
  error $ "invalid action " <> show n <> ": " <> show args
