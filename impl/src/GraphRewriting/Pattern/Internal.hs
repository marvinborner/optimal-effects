{-# LANGUAGE UnicodeSyntax #-}
module GraphRewriting.Pattern.Internal where

import GraphRewriting.Graph.Types
import Control.Monad.Reader
import Control.Monad.List


-- TODO: change the dependency of Match into a ReaderT Match?
-- | A pattern represents a graph scrutiny that memorises all the scrutinised nodes during matching.
newtype PatternT n m a = PatternT {patternT ∷ Match → ReaderT (Graph n) (ListT m) (Match, a)}

runPatternT' ∷ Match → PatternT n m a → Graph n → m [(Match,a)]
runPatternT' h p = runListT . runReaderT (patternT p h)

-- | Nodes matched in the evaluation of a pattern with the lastly matched node at the head
type Match = [Node]
