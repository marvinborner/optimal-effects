-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Front.Transformer.Lambda
  ( transformLambda
  ) where


import           Control.Monad.State            ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Data.Front                     ( Action(..)
                                                , Identifier
                                                , Term(..)
                                                )
import qualified Data.Lambda                   as Lambda
                                                ( Term(..) )
import           Data.List                      ( elemIndex )
import qualified Data.Text                     as T

type TransState = State [Identifier]

toChurch :: Int -> Lambda.Term
toChurch = Lambda.Abs . Lambda.Abs . go
 where
  go 0 = Lambda.Idx 0
  go n = Lambda.App (Lambda.Idx 1) (go (n - 1))

-- TODO: Consider different recursion transformation
yCombinator :: Lambda.Term
yCombinator = Lambda.Abs
  (Lambda.App
    (Lambda.Abs (Lambda.App (Lambda.Idx 0) (Lambda.Idx 0))) -- [0 0]
    (Lambda.Abs -- [1 (0 0)]
      (Lambda.App (Lambda.Idx 1) (Lambda.App (Lambda.Idx 0) (Lambda.Idx 0)))
    )
  )

isRecursive :: Identifier -> Term -> Bool
isRecursive n (Definition n' params body next)
  | n `elem` params = isRecursive n next
  | n == n'         = isRecursive n body
  | otherwise       = isRecursive n body || isRecursive n next
isRecursive n (If clause true false) =
  isRecursive n clause || isRecursive n true || isRecursive n false
isRecursive n (Var n') | n == n'   = True
                       | otherwise = False
isRecursive n (App a b     ) = isRecursive n a || isRecursive n b
isRecursive _ (Num _       ) = False
isRecursive n (Do  (Unit t)) = isRecursive n t
isRecursive n (Do (Bind n' t a))
  | n == n'   = isRecursive n t
  | otherwise = isRecursive n t || isRecursive n (Do a)

-- | Wraps term in n abstractions
wrap :: Int -> Lambda.Term -> Lambda.Term
wrap 0 t = t
wrap n t = Lambda.Abs $ wrap (n - 1) t

-- TODO: return Either
transform :: Term -> TransState Lambda.Term
transform (Definition n params body next)
  | isRecursive n body = do
    s <- get
    put (reverse (n : params) ++ s)
    b <- transform body
    put (n : s)
    d <- transform next
    let wrapped = wrap (length params) b
    return $ Lambda.App (Lambda.Abs d)
                        (Lambda.App yCombinator (Lambda.Abs wrapped))
  | otherwise = do
    s <- get
    put (reverse params ++ s)
    b <- transform body
    put (n : s)
    d <- transform next
    let wrapped = wrap (length params) b
    return $ Lambda.App (Lambda.Abs d) wrapped
transform (If clause true false) = do
  clause' <- transform clause
  true'   <- transform true
  false'  <- transform false
  return $ Lambda.App (Lambda.App clause' true') false'
transform (Var n) = do
  s <- get
  let maybeIdx = elemIndex n s
  return $ case maybeIdx of
    Nothing  -> error $ "not found " <> T.unpack n
    Just idx -> Lambda.Idx idx
transform (App a b) = do
  a' <- transform a
  b' <- transform b
  return $ Lambda.App a' b'
transform (Num n) = return $ toChurch n
transform t       = error $ show t
-- transform (Do as) = 

transformLambda :: Term -> Either String Lambda.Term
transformLambda t = Right $ evalState (transform t) []
