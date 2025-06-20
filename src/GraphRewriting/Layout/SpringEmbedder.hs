{-# LANGUAGE FlexibleContexts #-}
module GraphRewriting.Layout.SpringEmbedder where

import GraphRewriting.Graph
import GraphRewriting.Graph.Read
import GraphRewriting.Layout.Position
import GraphRewriting.Layout.PortSpec
import GraphRewriting.Layout.Rotation
import GraphRewriting.Layout.RotPortSpec
import GraphRewriting.Layout.Force
import Control.Monad
import Data.Vector.Class


springForce :: (View [Port] n, View Position n, View Rotation n, PortSpec n) => Double -> Node -> WithGraph n Force
springForce springLength n = liftM fsum $ mapM edgeForce =<< attachedEdges n where
	
	edgeForce e = do
		ns <- adverseNodes n e
		nTs <- springTargets e n
		nsTs <- liftM concat $ mapM (springTargets e) ns
		return $ fsum [attraction nsT nT | nsT <- nsTs, nT <- nTs]
	
	springTargets e node = do
		ps <- liftM (propOfPort absRotPortSpec e) (readNode node)
		return $ map (\(p,dir) -> p + vnormalise dir |* springLength) ps
