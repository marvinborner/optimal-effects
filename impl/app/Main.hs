-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Core                     as Core
import qualified Data.Front                    as Front
                                                ( Term(..) )
import qualified Data.Text                     as T
import           Debug.Trace
import qualified Language.Core.Reducer         as Core
                                                ( nf
                                                , visualize
                                                )
import qualified Language.Front.Parser         as Front
                                                ( parseProgram )
import qualified Language.Front.Transformer.Core
                                               as Front
                                                ( transformCore )
import qualified Language.Front.Transformer.Lambda
                                               as Front
                                                ( transformLambda )
import qualified Language.Lambda.Reducer       as Lambda
                                                ( nf )
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

-- pipeline :: T.Text -> Either String Core.Term
pipeline input = do
  front <- Front.parseProgram input
  Front.transformCore front
  -- lambda <- Front.transformLambda front
  -- return $ Lambda.nf
  --   (trace (show front <> "\n\n\n" <> show lambda <> "\n\n\n") lambda)

actions :: Args -> IO ()
actions Args { _argMode = ArgEval } = do
  program <- getContents
  case pipeline (T.pack program) of
    Left  err  -> putStrLn err
    Right core -> Core.visualize core
    -- Right out ->
    --   let term = show out
    --       -- normal = show $ nf out
    --   in  putStrLn $ term -- <> "\n" <> normal

main :: IO ()
main = execParser opts >>= actions
  where opts = info (args <**> helper) (fullDesc <> header "optimal effects")
