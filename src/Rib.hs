{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Rib where

import Control.Monad.IO.Class
import Data.Char ( ord )
import Data.Foldable ( foldrM )
import Data.IORef ( IORef )
import GHC.IO ( unsafePerformIO )

import Utils ( writeRef, readRef, newRef )

-- Rib Objects

data Rib = RibInt {-# UNPACK #-} !Int | RibRef {-# UNPACK #-} !(IORef RibObj)
  deriving (Eq)

instance Show Rib where
  show (RibInt n) = show n
  show (RibRef _) = "ref"

data RibObj = RibObj
  { field0 :: !Rib
  , field1 :: !Rib
  , field2 :: !Rib
  }
  deriving (Eq, Show)

-- Typeclass permettant l'utilisation de mkObj sans avoir à toujours créer des
-- IORef manuellement.
-- toRib permet la conversion d'une valeur de type a en IORef Rib par une action IO.
class ToRib a where
  toRib :: MonadIO m => a -> m Rib

-- Rien à faire, on a déjà un IORef Rib
instance ToRib Rib where
  toRib = pure

-- On crée une référence pour le Rib
instance ToRib RibObj where
  toRib = fmap RibRef . newRef

-- On wrap le Int dans RibInt et on crée une référence
instance ToRib Int where
  toRib = toRib . RibInt

-- On convertit le char en RibInt et on crée une référence
instance ToRib Char where
  toRib = toRib . RibInt . ord

mkObj :: (ToRib a, ToRib b, ToRib c, MonadIO m) => a -> b -> c -> m Rib
mkObj tag r1 r2 = toRib =<< RibObj <$> toRib r1 <*> toRib r2 <*> toRib tag

mkInstr :: (ToRib a, ToRib b, ToRib c, MonadIO m) => a -> b -> c -> m Rib
mkInstr tag r1 r2 = toRib =<< RibObj <$> toRib tag <*> toRib r1 <*> toRib r2

readN :: MonadIO m => (RibObj -> Rib) -> Rib -> m Rib
readN f (RibRef r) = f <$> readRef r
readN f (RibInt n) = error $ "readN: RibInt " <> show n <> " is not a pointer"

writeN :: MonadIO m => (RibObj -> RibObj) -> Rib -> m ()
writeN f (RibRef r) = readRef r >>= writeRef r . f
writeN f (RibInt n) = error $ "writeN: RibInt " <> show n <> " is not a pointer"

read0, read1, read2 :: MonadIO m => Rib -> m Rib
read0 = readN field0
read1 = readN field1
read2 = readN field2

write0, write1, write2 :: MonadIO m => Rib -> Rib -> m ()
write0 r v = writeN (\obj -> obj {field0 = v}) r
write1 r v = writeN (\obj -> obj {field1 = v}) r
write2 r v = writeN (\obj -> obj {field2 = v}) r

mkPair :: (ToRib a, ToRib b, MonadIO m) => a -> b -> m Rib
mkPair = mkObj (0 :: Int)

cons :: (ToRib a, ToRib b, MonadIO m) => a -> b -> m Rib
cons = mkPair

mkProc :: (ToRib a, ToRib b, MonadIO m) => a -> b -> m Rib
mkProc = mkObj (1 :: Int)

mkSymb :: (ToRib a, ToRib b, MonadIO m) => a -> b -> m Rib
mkSymb = mkObj (2 :: Int)

mkStr :: (ToRib a, ToRib b, MonadIO m) => a -> b -> m Rib
mkStr = mkObj (3 :: Int)

mkVect :: (ToRib a, ToRib b, MonadIO m) => a -> b -> m Rib
mkVect = mkObj (4 :: Int)

mkSVal :: MonadIO m => m Rib
mkSVal = mkObj (5 :: Int) (RibInt 0) (RibInt 0) -- Don't care about zeroes

{-# NOINLINE ribFalse #-}
ribFalse :: Rib
ribFalse = unsafePerformIO (toRib =<< mkSVal)

{-# NOINLINE ribTrue #-}
ribTrue :: Rib
ribTrue = unsafePerformIO (toRib =<< mkSVal)

{-# NOINLINE ribNil #-}
ribNil :: Rib
ribNil = unsafePerformIO (toRib =<< mkSVal)

-- -- Fonctions de conversion Haskell -> Rib

toRibList :: (ToRib a, MonadIO m) => [a] -> m Rib
toRibList = foldrM cons ribNil

toRibString :: MonadIO m => String -> m Rib
toRibString chars = do
  ribLst <- toRibList chars
  mkStr ribLst (length chars)

toRibSymbol :: MonadIO m => String -> m Rib
toRibSymbol chars = mkSymb (RibInt 0) =<< toRibString chars
