{-# LANGUAGE TupleSections, FlexibleInstances #-}

module Utils where

import Control.Monad.IO.Class

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

-- ReaderIO permet de modéliser le passage de paramètre constant, similaire au
-- dependency injection.
-- En Haskell, cela est fait en prenant comme argument le paramètre (de type r)
-- et en le passant à la continuation.
-- Pour s'aider à l'utiliser, on donne une instance Monad à ReaderIO.
-- Comment lire la signature de type:
--  r est le type du paramètre
--  a est le type de retour
-- Exemple:
--   1. `ReaderIO Int Int` représente une action qui prend Int en entrée et retourne un Int.
--      De plus, il peut faire de l'IO.
--      Exemple d'action avec ce type:
--        a. pure 5: Retourne toujours 5
--        b. ReaderIO id: Retourne toujours le paramètre
--        b. ReaderIO (\r -> do print r; getInt):
--          Affiche le paramètre, retourne un int obtenu en faisant une opération IO.
newtype ReaderIO r a = ReaderIO { runReaderIO :: r -> IO (r, a) }

-- Cette action permet d'obtenir le paramètre.
get :: ReaderIO r r
get = ReaderIO (\s -> pure (s, s))

set :: r -> ReaderIO r ()
set s = ReaderIO (\_ -> pure (s, ()))

-- Une Monad est un Applicative est un Functor.
-- On doit donc définir une instance de Functor d'abord.
-- Functor est la généralisation de la fonction map sur les listes.
-- Un foncteur f "contenant" des valeurs de type `a` permet de mapper chacun de
-- ces `a` pour obtenir une valeur de type b correspondante.
instance Functor (ReaderIO r) where
  -- Signature: fmap :: (a -> b) -> ReaderIO r a -> ReaderIO r b
  fmap f (ReaderIO r) = ReaderIO $ \s ->
    r s >>= \(s', a) -> pure (s', f a)

-- Applicative permet le séquencement d'opération sans branchement.
instance Applicative (ReaderIO r) where
  -- pure :: a -> ReaderT r m a
  -- pure retourne toujours la même valeur en ignorant le paramètre
  pure a = ReaderIO $ \s -> pure (s, a)
  -- (<*>) :: ReaderIO r m (a -> b) -> ReaderIO r m a -> ReaderIO r m b
  (ReaderIO f) <*> (ReaderIO a) = ReaderIO $ \s -> do
    (s', f') <- f s    -- On exécute f :: IO (a -> b) qui nous donne f' :: a -> b
    (s'', a') <- a s'    -- Idem pour a :: IO a qui donne a' :: a
    pure (s'', f' a') -- On retourne l'application f' a'

instance Monad (ReaderIO r) where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (ReaderIO f) >>= cont = ReaderIO $ \s -> do
    f s >>= \(s', a) -> runReaderIO (cont a) s'

-- Action permettant l'échec d'une action.
-- On s'en sert ici quand on laisse un pattern match vide.
instance MonadFail (ReaderIO r) where
  -- fail :: String -> ReaderIO r a
  fail = liftIO . fail

-- Pour faciliter l'utilisation d'action de type IO et ReaderIO.

instance MonadIO (ReaderIO r) where
  liftIO io = ReaderIO $ \s -> (s,) <$> io

newRef :: MonadIO m => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: MonadIO m => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: MonadIO m => IORef a -> a -> m ()
writeRef ref = liftIO . writeIORef ref
