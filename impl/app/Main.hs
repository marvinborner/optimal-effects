-- MIT License, Copyright (c) 2024 Marvin Borner

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Core                      ( Term(..) )
import qualified Data.Text                     as T
import           Language.Core.Parser           ( parseProgram )
import           Language.Core.Reducer          ( nf )
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

pipeline :: T.Text -> Either String Term
pipeline program = parseProgram program -- >>= linearity >>= typeCheck

actions :: Args -> IO ()
actions Args { _argMode = ArgEval } = do
  program <- getContents
  case pipeline (T.pack program) of
    Left err -> putStrLn err
    Right out ->
      let term   = show out
          normal = show $ nf out
      in  putStrLn $ term <> "\n" <> normal

main :: IO ()
main = execParser opts >>= actions
  where opts = info (args <**> helper) (fullDesc <> header "optimal effects")
