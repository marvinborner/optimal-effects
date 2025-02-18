-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Front                    as Front
                                                ( Term(..) )
import qualified Data.LambdaScope              as LambdaScope
import qualified Data.Text                     as T
import           Debug.Trace
import qualified Language.Front.Parser         as Front
                                                ( parseProgram )
import qualified Language.Front.Transformer.Lambda
                                               as Front
                                                ( transformLambda )
import qualified Language.Front.Transformer.LambdaScope
                                               as Front
                                                ( transformLambdaScope )
import qualified Language.Lambda.Reducer       as Lambda
                                                ( nf )
import qualified Language.LambdaScope.Reducer  as LambdaScope
                                                ( nf
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

-- pipeline :: T.Text -> Either String LambdaScope.Term
pipeline input = do
  front <- Front.parseProgram input
  -- lambda <- Front.transformLambda front
  -- let lnf = traceShow $ Lambda.nf
  --       (trace (show front <> "\n\n\n" <> show lambda <> "\n\n\n") lambda)
  Front.transformLambdaScope front

actions :: Args -> IO ()
actions Args { _argMode = ArgEval } = do
  program <- getContents
  case pipeline (T.pack program) of
    Left  err  -> putStrLn err
    Right core -> LambdaScope.visualize core
    -- Right out ->
    --   let term = show out
    --       -- normal = show $ nf out
    --   in  putStrLn $ term -- <> "\n" <> normal

main :: IO ()
main = execParser opts >>= actions
  where opts = info (args <**> helper) (fullDesc <> header "optimal effects")
