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

data ArgMode = Visualize | Benchmark | BenchmarkRandom
  deriving Eq

data Args = Args
  { _argMode   :: ArgMode
  , _argTarget :: String
  }

mode :: Parser ArgMode
mode =
  flag' Visualize (long "visualize" <> short 'v' <> help "Visualize reduction")
    <|> flag' Benchmark
              (long "benchmark" <> short 'b' <> help "Benchmark reduction")
    <|> flag'
          BenchmarkRandom
          (long "benchmark-random" <> short 'r' <> help
            "Benchmark reduction with random rules"
          )

args :: Parser Args
args = Args <$> (mode <|> pure Visualize) <*> strOption
  (long "target" <> short 't' <> value "target" <> help
    "target backend (direct, monad)"
  )

lambdaPipeline target input = do
  front  <- Front.parseProgram input
  lambda <- Front.transformLambda front
  trace (show front <> "\n\n\n" <> show (Lambda.unwrap lambda)) (pure lambda)

monadPipeline Visualize =
  either putStrLn Monad.visualize . Lambda.transformMonad
monadPipeline mode = either putStrLn bench . Lambda.transformMonad
  where bench = Monad.bench (mode == BenchmarkRandom)

directPipeline Visualize =
  either putStrLn Direct.visualize . Lambda.transformDirect
directPipeline mode = either putStrLn bench . Lambda.transformDirect
  where bench = Direct.bench (mode == BenchmarkRandom)

actions :: Args -> IO ()
actions Args { _argMode = mode, _argTarget = target } = do
  program <- getContents
  case lambdaPipeline target $ T.pack program of
    Left err -> putStrLn err
    Right lambda | target == "monad" -> monadPipeline mode lambda
                 | target == "direct" -> directPipeline mode lambda
                 | otherwise -> putStrLn "target must be monad or direct"

main :: IO ()
main = execParser opts >>= actions
  where opts = info (args <**> helper) (fullDesc <> header "optimal effects")
