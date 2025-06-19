-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Front                    as Front
                                                ( Term(..) )
import qualified Data.Lambda                   as Lambda
import qualified Data.Text                     as T
import qualified Data.Direct             as Direct
import           Debug.Trace
import qualified Language.Front.Parser         as Front
                                                ( parseProgram )
import qualified Language.Front.Transformer.Lambda
                                               as Front
                                                ( transformLambda )
import qualified Language.Lambda.Transformer.Monad
                                               as Lambda
                                                ( transformMonad )
import qualified Language.Lambda.Transformer.Direct
                                               as Lambda
                                                ( transformDirect )
import qualified Language.Monad.Reducer        as Monad
                                                ( bench
                                                , visualize
                                                )
import qualified Language.Direct.Reducer as Direct
                                                ( bench
                                                , visualize
                                                )
import           Options.Applicative            ( (<**>)
                                                , Parser
                                                , execParser
                                                , fullDesc
                                                , header
                                                , helper
                                                , info
                                                )

data ArgMode = ArgEval

newtype Args = Args
  { _argMode :: ArgMode
  }

args :: Parser Args
args = pure $ Args ArgEval

-- pipeline :: T.Text -> Either String Direct.Term
pipeline input = do
  front  <- Front.parseProgram input
  lambda <- Front.transformLambda front
  trace (show front <> "\n\n\n" <> show (Lambda.unwrap lambda))
        (Lambda.transformMonad lambda)

actions :: Args -> IO ()
actions Args { _argMode = ArgEval } = do
  program <- getContents
  case pipeline $ T.pack program of
    Left  err  -> putStrLn err
    Right core -> Monad.visualize core
      -- Monad.visualize core

main :: IO ()
main = execParser opts >>= actions
  where opts = info (args <**> helper) (fullDesc <> header "optimal effects")
