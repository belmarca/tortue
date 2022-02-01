{-# LANGUAGE TupleSections #-}

module Utils where

import Control.Monad.IO.Class
import Data.IORef

-- StateIO permet de modéliser le passage de paramètre constant, similaire au
-- dependency injection.
-- En Haskell, cela est fait en prenant comme argument le paramètre (de type r)
-- et en le passant à la continuation.
-- Pour s'aider à l'utiliser, on donne une instance Monad à StateIO.
-- Comment lire la signature de type:
--  r est le type du paramètre
--  a est le type de retour
-- Exemple:
--   1. `StateIO Int Int` représente une action qui prend Int en entrée et retourne un Int.
--      De plus, il peut faire de l'IO.
--      Exemple d'action avec ce type:
--        a. pure 5: Retourne toujours 5
--        b. StateIO id: Retourne toujours le paramètre
--        b. StateIO (\r -> do print r; getInt):
--          Affiche le paramètre, retourne un int obtenu en faisant une opération IO.
newtype StateIO r a = StateIO { runReaderIO :: r -> IO (r, a) }

-- Cette action permet d'obtenir le paramètre.
get :: StateIO r r
get = StateIO (\s -> pure (s, s))

set :: r -> StateIO r ()
set s = StateIO (\_ -> pure (s, ()))

-- Une Monad est un Applicative est un Functor.
-- On doit donc définir une instance de Functor d'abord.
-- Functor est la généralisation de la fonction map sur les listes.
-- Un foncteur f "contenant" des valeurs de type `a` permet de mapper chacun de
-- ces `a` pour obtenir une valeur de type b correspondante.
instance Functor (StateIO r) where
  -- Signature: fmap :: (a -> b) -> StateIO r a -> StateIO r b
  fmap f (StateIO r) = StateIO $ \s ->
    r s >>= \(s', a) -> pure (s', f a)

-- Applicative permet le séquencement d'opération sans branchement.
instance Applicative (StateIO r) where
  -- pure :: a -> ReaderT r m a
  -- pure retourne toujours la même valeur en ignorant le paramètre
  pure a = StateIO $ \s -> pure (s, a)
  -- (<*>) :: StateIO r m (a -> b) -> StateIO r m a -> StateIO r m b
  (StateIO f) <*> (StateIO a) = StateIO $ \s -> do
    (s', f') <- f s    -- On exécute f :: IO (a -> b) qui nous donne f' :: a -> b
    (s'', a') <- a s'    -- Idem pour a :: IO a qui donne a' :: a
    pure (s'', f' a') -- On retourne l'application f' a'

instance Monad (StateIO r) where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (StateIO f) >>= cont = StateIO $ \s -> do
    f s >>= \(s', a) -> runReaderIO (cont a) s'

-- Action permettant l'échec d'une action.
-- On s'en sert ici quand on laisse un pattern match vide.
instance MonadFail (StateIO r) where
  -- fail :: String -> StateIO r a
  fail = liftIO . fail

-- Pour faciliter l'utilisation d'action de type IO et StateIO.

instance MonadIO (StateIO r) where
  liftIO io = StateIO $ \s -> (s,) <$> io

newRef :: MonadIO m => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: MonadIO m => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: MonadIO m => IORef a -> a -> m ()
writeRef ref = liftIO . writeIORef ref
