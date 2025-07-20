-- MIT License, Copyright (c) 2025 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Direct                   as Direct
import qualified Data.Front                    as Front
                                                ( Term(..) )
import qualified Data.Lambda                   as Lambda
import qualified Data.Text                     as T
import           Data.View
import           Debug.Trace
import           GraphRewriting.Graph
import           GraphRewriting.Graph.Types
import qualified Language.Direct.Reducer       as Direct
import qualified Language.Front.Parser         as Front
import qualified Language.Front.Transformer.Lambda
                                               as Front
import qualified Language.Lambda.Transformer.Direct
                                               as Lambda
import qualified Language.Lambda.Transformer.Monad
                                               as Lambda
import qualified Language.Monad.Reducer        as Monad
import           Options.Applicative

data ArgMode = Visualize | Benchmark
  deriving Eq

data Args = Args
  { _argMode   :: ArgMode
  , _argTarget :: String
  , _infer     :: Bool
  , _random    :: Bool
  , _parallel  :: Bool
  }

mode :: Parser ArgMode
mode =
  flag' Visualize (long "visualize" <> short 'v' <> help "Visualize reduction")
    <|> flag' Benchmark
              (long "benchmark" <> short 'b' <> help "Benchmark reduction")

args :: Parser Args
args =
  Args
    <$> (mode <|> pure Visualize)
    <*> strOption
          (long "target" <> short 't' <> value "direct" <> help
            "target backend (direct, monad)"
          )
    <*> switch
          (  long "infer"
          <> short 'i'
          <> help
               "Use inference for direct style (ie. rotate redirectors left). Increases parallelism."
          )
    <*> switch
          (  long "random"
          <> short 'r'
          <> help
               "Apply rules in random order (while benchmarking). Should result in constant interaction counts."
          )
    <*> switch
          (  long "parallel"
          <> short 'p'
          <> help
               "Simulate parallel reduction by applying rules exhaustively instead of once (while benchmarking). Counts all possible parallel reductions per rule as 1. Will not have constant interaction counts."
          )

lambdaPipeline target input = do
  front  <- Front.parseProgram input
  lambda <- Front.transformLambda front
  trace (show front <> "\n\n\n" <> show (Lambda.unwrap lambda)) (pure lambda)

monadPipeline infer random parallel mode =
  either putStrLn (func mode infer random parallel) . Lambda.transformMonad
 where
  func Visualize = Monad.visualize
  func Benchmark = Monad.bench

directPipeline infer random parallel mode =
  either putStrLn (func mode infer random parallel) . Lambda.transformDirect dir
 where
  func Visualize = Direct.visualize
  func Benchmark = Direct.bench
  dir = case infer of -- TODO: this should absolutely not be here
    True  -> Direct.BottomLeft
    False -> Direct.BottomRight

actions :: Args -> IO ()
actions Args { _argMode = mode, _argTarget = target, _infer = infer, _random = random, _parallel = parallel }
  = do
    program <- getContents
    case lambdaPipeline target $ T.pack program of
      Left err -> putStrLn err
      Right lambda
        | target == "monad"  -> monadPipeline infer random parallel mode lambda
        | target == "direct" -> directPipeline infer random parallel mode lambda
        | otherwise          -> putStrLn "target must be monad or direct"

main :: IO ()
main = execParser opts >>= actions
  where opts = info (args <**> helper) (fullDesc <> header "optimal effects")
