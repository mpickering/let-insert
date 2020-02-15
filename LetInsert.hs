{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module LetInsert where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Language.Haskell.TH
import qualified Control.Monad.Writer as W
import Data.Functor.Identity
import Data.Maybe

data WrappedCode m where
  WrappedCode :: Code m b -> WrappedCode m

type LetT m a = WriterT [(WrappedCode m, Name, Name)] m a

type Code m a = m (TExp a)

type LetCode m a = LetT m (TExp a)

data Locus = Locus

instance (Monoid w, Quote m) => Quote (WriterT w m) where
  newName x = W.lift (newName x)

instance (Monoid w, Quote m) => Quote (StateT w m) where
  newName x = W.lift (newName x)



locus :: (Quote m, Monad m) => (Locus -> LetCode m a) -> Code m a
locus a = do
  (a, bs) <- runWriterT (a Locus)
  unsafeTExpCoerce (LetE <$> generateBindings bs <*> pure (unType a))
  where
    generateBindings = traverse makeDecl
      where
        makeDecl (WrappedCode body, n, a) =
          do
             body' <- unTypeQ body
             return (FunD n [Clause [VarP a] (NormalB body') []])

newTypedName :: Quote m => m (TExp a)
newTypedName = do
  n <- newName "n"
  return (TExp (VarE n))


gen :: Quote m => Locus -> (Code Identity (a -> b) -> LetCode m a -> LetCode m b) -> LetCode m (a -> b)
gen l f = do
  n <- newName "n"
  let n' = TExp (VarE n)

  a <- newName "a"
  let ta = return (TExp (VarE a))
  body <- f (Identity n') ta
  tell [(WrappedCode (return body), n, a)]
  return n'


mrfix :: forall a b m r . (Monad m, Ord a, Quote m)
      => (forall m . Quote m => (a -> Code m (b -> r)) -> (a -> Code m b -> Code m r))
      -> (a -> Code m (b -> r))
mrfix f x =
  flip evalStateT Map.empty $
    locus $ \locus -> do
      let loop :: a -> WriterT
                        [(WrappedCode (StateT (Map.Map a (Identity (TExp (b -> r)))) m),
                          Name, Name)]
                        (StateT (Map.Map a (Identity (TExp (b -> r)))) m)
                        (TExp (b -> r))
          loop n = do
            m <- get
            case Map.lookup n m of
              Just (Identity v) -> return v
              Nothing -> do
                gen locus (\g y -> do
                  modify (Map.insert n g)
                  f loop n y)
      loop x


data Token = A | B deriving Eq
data State = S | T | U deriving (Eq, Ord)
data Auto a s = Auto { finals :: [s], trans :: [(s, [(a, s)])] }


makeau :: (Eq s, Quote m) => Auto Token s ->
       (s -> (Code m ([Token] -> Bool)))
       -> s
       -> Code m [Token]
       -> Code m Bool
makeau (Auto f t) self state stream =
  let accept = state `elem` f
      next token = fromMaybe (error "err") $ lookup token (fromMaybe (error "err") $ lookup state t)
  in [|| case $$(stream) of
            A : r -> $$(self (next A)) r
            B : r -> $$(self (next B)) r
            [] -> accept ||]


au1 = Auto [S] [(S, [(A, S), (B, T)])
               ,(T, [(A, S), (B, U)])
               ,(U, [(A, T), (B, U)])]

res :: Q (TExp ([Token] -> Bool))
res = mrfix (makeau au1) S

