{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Spicy.Wrapper.Internal.Input.Templates () where

{-
Previously, Spicy used mustache templates to construct input files.
This had the advantage of allowing for a single function consistent
through programs. However, Text-templates are "stupid":
There is no safety 

This approach consist of specifying a "language" using a functor
and an interpretation function, specifying the structure of the file monadically,
and then interpreting into the final text.
-}

import RIO

data Free f a
  = Done a
  | Free (f (Free f a))
  deriving (Functor)

instance (Show a, forall x. Show x => Show (f x)) => Show (Free f a) where
  show (Done a) = "Done " <> show a
  show (Free f) = "Free (" <> show f <> ")"

instance Functor f => Applicative (Free f) where
  pure = Done
  Done f <*> fb = f <$> fb
  Free ffree <*> fb = Free $ (<*> fb) <$> ffree

instance (Functor f) => Monad (Free f) where
  return = pure
  Done a >>= f = f a
  Free ffree >>= f = Free $ (>>= f) <$> ffree
liftF :: Functor f => f a -> Free f a
liftF f = Free $ return <$> f

runFree :: (Functor f, Monoid m) => (f m -> m) -> Free f a -> m
runFree _ (Done _) = mempty
runFree f (Free g) = f (runFree f <$> g)

--------------------------------------------------------------------------------

data XTBInput a
  = Charge Int a
  | Spin Int a
  | Method Int a
  deriving (Functor, Show)


xtbStandardInput :: Int -> Int -> Int -> Free XTBInput ()
xtbStandardInput chrg spn mthd = do
  _ <- liftF $ Charge chrg ()
  _ <- liftF $ Spin spn ()
  _ <- liftF $ Method mthd ()
  return ()

compileXTB :: XTBInput Text -> Text
compileXTB (Charge c t) = "$chrg=" <> tshow c <> "\n" <> t
compileXTB (Spin s t) = "$spin=" <> tshow s <> "\n" <> t
compileXTB (Method m t) = "$method=" <> tshow m <> "\n" <> t

--------------------------------------------------------------------------------

data Psi4Input a
  = Memory Int a
  | Molecule Int Int Text a
  | Set String a
  | Hessian a
  | Fchk String a
  deriving (Functor)

psi4StandardInput :: Free Psi4Input ()
psi4StandardInput = do
  liftF $ Memory 4000 ()
  liftF $ Molecule 1 2 "test" ()
  when True . liftF $ Hessian ()