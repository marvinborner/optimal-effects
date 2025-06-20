{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module GraphRewriting.Layout.Coulomb where

import Data.View
import Data.Functor ()
import GraphRewriting.Graph.Types
import GraphRewriting.Graph.Read
import GraphRewriting.Pattern ()
import GraphRewriting.Layout.Position
import GraphRewriting.Layout.Force


coulombForce ∷ View Position n ⇒ Node -> WithGraph n Force
coulombForce node = do
 	n ← examine position <$> readNode node
 	ns ← fmap (map $ examine position) (mapM readNode =<< readNodeList)
 	return $ fsum [repulsion n' n | n' ← ns]
