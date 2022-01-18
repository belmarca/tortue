module Utils where
import Data.IORef

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
newtype ReaderIO r a = ReaderIO { runReaderIO :: r -> IO a }
  -- deriving newtype (Functor, Monad, Applicative)

-- Cette action permet d'obtenir le paramètre.
get :: ReaderIO r r
get = ReaderIO pure

-- Une Monad est un Applicative est un Functor.
-- On doit donc définir une instance de Functor d'abord.
-- Functor est la généralisation de la fonction map sur les listes.
-- Un foncteur f "contenant" des valeurs de type `a` permet de mapper chacun de
-- ces `a` pour obtenir une valeur de type b correspondante.
instance Functor (ReaderIO r) where
  -- Signature: fmap :: (a -> b) -> ReaderIO r a -> ReaderIO r b
  fmap f (ReaderIO r) = ReaderIO (\a -> fmap f (r a))

-- Applicative permet le séquencement d'opération sans branchement.
instance Applicative (ReaderIO r) where
  -- pure :: a -> ReaderT r m a
  -- pure retourne toujours la même valeur en ignorant le paramètre
  pure a = ReaderIO $ \_ -> pure a
  -- (<*>) :: ReaderIO r m (a -> b) -> ReaderIO r m a -> ReaderIO r m b
  (ReaderIO f) <*> (ReaderIO a) = ReaderIO $ \r -> do
    f' <- f r    -- On exécute f :: IO (a -> b) qui nous donne f' :: a -> b
    a' <- a r    -- Idem pour a :: IO a qui donne a' :: a
    pure (f' a') -- On retourne l'application f' a'

instance Monad (ReaderIO r) where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (ReaderIO f) >>= cont = ReaderIO $ \r -> do
    f r >>= flip runReaderIO r . cont

-- Action permettant l'échec d'une action.
-- On s'en sert ici quand on laisse un pattern match vide.
instance MonadFail (ReaderIO r) where
  -- fail :: String -> ReaderIO r a
  fail = liftIO . fail

-- Pour faciliter l'utilisation d'action de type IO et ReaderIO.
class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

instance MonadIO (ReaderIO r) where
  liftIO = ReaderIO . const

newRef :: MonadIO m => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: MonadIO m => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: MonadIO m => IORef a -> a -> m ()
writeRef ref = liftIO . writeIORef ref

-- Ignore la valeur retournée par l'action
void :: Monad m => m a -> m ()
void = fmap (const ())
