{-# LANGUAGE NoMonomorphismRestriction #-}
module Rib where

import Data.Char
import Data.Foldable
import Data.IORef
import GHC.IO
import Utils

-- Rib Objects

data Rib = RibInt {-# UNPACK #-} !Int | RibRef {-# UNPACK #-} !(IORef RibObj) deriving (Eq)

data RibObj = RibObj { field0 :: !Rib, field1 :: !Rib, field2 :: !Rib} deriving (Eq)

-- Typeclass permettant l'utilisation de mkObj sans avoir à toujours créer des
-- IORef manuellement.
-- toRib permet la conversion d'une valeur de type a en IORef Rib par une action IO.
class ToRib a where toRib :: a -> IO Rib

-- Rien à faire, on a déjà un IORef Rib
instance ToRib Rib where toRib = pure

-- On crée une référence pour le Rib
instance ToRib RibObj where toRib = fmap RibRef . newRef

-- On wrap le Int dans RibInt et on crée une référence
instance ToRib Int where toRib = toRib . RibInt

-- On convertit le char en RibInt et on crée une référence
-- instance ToRib Char where toRib = toRib . RibInt . ord

-- mkObj :: (ToRib a, ToRib b, ToRib c) => a -> b -> c -> IO Rib -- Debug
mkObj tag r1 r2 = toRib =<< RibObj <$> toRib r1 <*> toRib r2 <*> toRib tag

-- mkInstr :: (ToRib a, ToRib b, ToRib c) => a -> b -> c -> IO Rib -- Debug
mkInstr tag r1 r2 = toRib =<< RibObj <$> toRib tag <*> toRib r1 <*> toRib r2

-- readN :: (RibObj -> Rib) -> Rib -> IO Rib -- Debug
readN f (RibRef r) = f <$> readRef r
-- readN f (RibInt n) = error $ "readN: RibInt " <> show n <> " is not a pointer" -- Debug

-- writeN :: (RibObj -> RibObj) -> Rib -> IO () -- Debug
writeN f (RibRef r) = readRef r >>= writeRef r . f
-- writeN f (RibInt n) = error $ "writeN: RibInt " <> show n <> " is not a pointer" -- Debug

-- read0, read1, read2 :: Rib -> IO Rib -- Debug
read0 = readN field0
read1 = readN field1
read2 = readN field2

-- write0, write1, write2 :: Rib -> Rib -> IO () -- Debug
write0 r v = writeN (\obj -> obj {field0 = v}) r
write1 r v = writeN (\obj -> obj {field1 = v}) r
write2 r v = writeN (\obj -> obj {field2 = v}) r

-- cons, mkProc, mkSymb, mkStr, mkVect :: (ToRib a, ToRib b) => a -> b -> IO Rib -- Debug
cons = mkObj (RibInt 0)
mkProc = mkObj (RibInt 1)
mkSymb = mkObj (RibInt 2)
mkStr  = mkObj (RibInt 3)
mkVect = mkObj (RibInt 4)

-- mkSVal :: IO Rib -- Debug
mkSVal = mkObj (RibInt 5) (RibInt 0) (RibInt 0) -- Don't care about zeroes

{-# NOINLINE ribFalse #-}
-- ribFalse, ribTrue, ribNil :: Rib -- Debug
ribFalse = unsafePerformIO (toRib =<< mkSVal)

{-# NOINLINE ribTrue #-}
ribTrue = unsafePerformIO (toRib =<< mkSVal)

{-# NOINLINE ribNil #-}
ribNil = unsafePerformIO (toRib =<< mkSVal)

-- Fonctions de conversion Haskell -> Rib

-- toRibList :: (ToRib a) => [a] -> IO Rib -- Debug
toRibList = foldrM cons ribNil

-- toRibString :: String -> IO Rib -- Debug
toRibString chars = toRibList (ord <$> chars) >>= flip mkStr (length chars)

-- toRibSymbol :: String -> IO Rib -- Debug
toRibSymbol = (=<<) (mkSymb (RibInt 0)) . toRibString
