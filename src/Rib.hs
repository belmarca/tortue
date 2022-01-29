{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-} -- Rend le langage strict pour de meilleures performances
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Rib where

import Data.Char ( ord )
import Data.Foldable ( foldrM )
import Data.IORef ( IORef )
import GHC.IO ( unsafePerformIO )

import Utils ( writeRef, readRef, newRef, MonadIO )
import GHC.Stack ( HasCallStack )

-- Rib Objects

data Rib = RibInt Int | RibObj (IORef Rib) (IORef Rib) (IORef Rib)

-- Typeclass permettant l'utilisation de mkObj sans avoir à toujours créer des
-- IORef manuellement.
-- toRibRef permet la conversion d'une valeur de type a en IORef Rib par une action IO.
class RibRef a where
  toRibRef :: MonadIO m => a -> m (IORef Rib)

-- Rien à faire, on a déjà un IORef Rib
instance RibRef (IORef Rib) where
  toRibRef = pure

-- On crée une référence pour le Rib
instance RibRef Rib where
  toRibRef = newRef

-- On wrap le Int dans RibInt et on crée une référence
instance RibRef Int where
  toRibRef = newRef . RibInt

-- On convertit le char en RibInt et on crée une référence
instance RibRef Char where
  toRibRef = newRef . RibInt . ord

mkObj :: (RibRef a, RibRef b, RibRef c, MonadIO m) => a -> b -> c -> m Rib
mkObj tag r1 r2 = RibObj <$> toRibRef r1 <*> toRibRef r2 <*> toRibRef tag

mkInstr :: (RibRef a, RibRef b, RibRef c, MonadIO m) => a -> b -> c -> m Rib
mkInstr tag r1 r2 = RibObj <$> toRibRef tag <*> toRibRef r1 <*> toRibRef r2

read1, read2, read3 :: (MonadIO m, HasCallStack) => Rib -> m Rib
read1 (RibObj v _ _) = readRef v
read2 (RibObj _ v _) = readRef v
read3 (RibObj _ _ v) = readRef v

write1, write2, write3 :: (MonadIO m, HasCallStack) => Rib -> Rib -> m ()
write1 (RibObj v _ _) = writeRef v
write2 (RibObj _ v _) = writeRef v
write3 (RibObj _ _ v) = writeRef v

mkPair :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkPair = mkObj (0 :: Int)

cons :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
cons = mkPair

mkProc :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkProc = mkObj (1 :: Int)

mkSymb :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkSymb = mkObj (2 :: Int)

mkStr :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkStr = mkObj (3 :: Int)

mkVect :: (RibRef a, RibRef b, MonadIO m) => a -> b -> m Rib
mkVect = mkObj (4 :: Int)

mkSVal :: MonadIO m => m Rib
mkSVal = mkObj (5 :: Int) (RibInt 0) (RibInt 0) -- Don't care about zeroes

{-# NOINLINE ribFalse #-}
ribFalse :: Rib
ribFalse = unsafePerformIO mkSVal

{-# NOINLINE ribTrue #-}
ribTrue :: Rib
ribTrue = unsafePerformIO mkSVal

ribNil :: Rib
ribNil = unsafePerformIO mkSVal

-- Fonctions de conversion Haskell -> Rib

toRibList :: (RibRef a, MonadIO m) => [a] -> m Rib
toRibList = foldrM cons ribNil

toRibString :: MonadIO m => String -> m Rib
toRibString chars = do
  ribLst <- toRibList chars
  mkStr ribLst (length chars)

toRibSymbol :: MonadIO m => String -> m Rib
toRibSymbol chars = mkSymb (RibInt 0) =<< toRibString chars
