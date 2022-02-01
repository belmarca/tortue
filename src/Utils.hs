{-# LANGUAGE TupleSections, NoMonomorphismRestriction #-}

module Utils where

import Control.Monad.IO.Class
import Data.IORef

-- StateIO permet de modéliser la mutation "pure" en Haskell.
newtype StateIO r a = StateIO { runReaderIO :: r -> IO (r, a) }

-- Cette action permet d'obtenir le paramètre.
-- get :: StateIO r r -- Debug
get = StateIO (\s -> pure (s, s))

-- set :: r -> StateIO r () -- Debug
set s = StateIO (\_ -> pure (s, ()))

-- Une Monad est un Applicative est un Functor.
-- On doit donc définir une instance de Functor d'abord.
-- Functor est la généralisation de la fonction map sur les listes.
-- Un foncteur f "contenant" des valeurs de type `a` permet de mapper chacun de
-- ces `a` pour obtenir une valeur de type b correspondante.
instance Functor (StateIO r) where
  fmap f (StateIO r) = StateIO $ fmap (fmap f) . r

-- Applicative permet le séquencement d'opération sans branchement.
instance Applicative (StateIO r) where
  pure a = StateIO $ \s -> pure (s, a)
  (StateIO f) <*> (StateIO a) = StateIO $ \s -> do
    (s', f') <- f s
    fmap f' <$> a s'

-- Applicative permet le séquencement d'opération avec branchement.
instance Monad (StateIO r) where
  (StateIO f) >>= cont = StateIO $ \s -> do
    f s >>= \(s', a) -> runReaderIO (cont a) s'

-- Action permettant l'échec d'une action.
-- On s'en sert ici quand on laisse un pattern match vide.
instance MonadFail (StateIO r) where fail = liftIO . fail

-- Pour faciliter l'utilisation d'action de type IO et StateIO.

instance MonadIO (StateIO r) where liftIO m = StateIO $ \s -> (s,) <$> m

-- newRef :: MonadIO m => a -> m (IORef a) -- Debug
newRef = liftIO . newIORef

-- readRef :: MonadIO m => IORef a -> m a -- Debug
readRef = liftIO . readIORef

-- writeRef :: MonadIO m => IORef a -> a -> m () -- Debug
writeRef ref = liftIO . writeIORef ref
