{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module GraphRewriting.Layout.Gravitation where

import Data.View
import Data.Vector.Class
import Data.Vector.V2 ()
import Data.Functor ()
import GraphRewriting.Pattern ()
import GraphRewriting.Graph.Types
import GraphRewriting.Graph.Read
import GraphRewriting.Layout.Position
import GraphRewriting.Layout.Force


centralGravitation ∷ View Position n ⇒ Node → WithGraph n Force
centralGravitation = fmap (attraction (vpromote 0) . examine position) . readNode

gravitation ∷ View Position n ⇒ Node -> Rewrite n Force
gravitation node = do
 	n ← examine position <$> readNode node
 	ns ← fmap (map $ examine position) (mapM readNode =<< readNodeList)
 	return $ fsum [attraction n' n | n' ← ns]
