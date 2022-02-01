{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
module Debug where

import Control.Monad.IO.Class
import Data.Aeson ( KeyValue((.=)) )
import Data.Char ( chr, ord )
import Data.Text ( Text, pack )
import Data.IORef ( IORef )
import Data.Scientific ( floatingOrInteger )
import GHC.IO ( unsafePerformIO )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vector

import Utils
import Rib
import Env

-- Debugging functions

printRib :: Rib -> SIO ()
printRib r = liftIO . BS.putStrLn . Aeson.encodePretty =<< ribDataToJson r

printInstrRib :: Rib -> SIO ()
printInstrRib r = liftIO . BS.putStrLn . Aeson.encodePretty =<< ribInstructionToJson r

printFirstInstrRib :: Rib -> SIO ()
printFirstInstrRib r = liftIO . BS.putStrLn . Aeson.encodePretty . head =<< ribInstructionToJson r

printRibList :: Rib -> SIO ()
printRibList (RibRef r) = do
  RibObj car cdr _ <- readRef r
  liftIO . BS.putStrLn . Aeson.encodePretty =<< decodeList car cdr
printRibList r@(RibInt n) = printRib r

{-# NOINLINE testRibLst #-}
testRibLst :: Rib
testRibLst = unsafePerformIO $ toRibList [ord 'a', 4, 3, 2, 1]

{-# NOINLINE testRibStr #-}
testRibStr :: Rib
testRibStr = unsafePerformIO $ toRibString "abc"

{-# NOINLINE testRibSymb #-}
testRibSymb :: Rib
testRibSymb = unsafePerformIO $ toRibSymbol "abc"

{-# NOINLINE testRibLst2 #-}
testRibLst2 :: Rib
testRibLst2 = unsafePerformIO $ toRibList [testRibStr, testRibSymb, ribNil]

data Sexp = SexpLoop Int | SexpInt Int | SexpLst Int Sexp Sexp Sexp
  deriving (Eq)

instance Show Sexp where
  show (SexpLoop n) = "[...]" -- "#" <> show n
  show (SexpInt n) = show n
  show (SexpLst ix s1 s2 s3) = show [s1, s2, s3]
    -- "[#" <> show ix <> "," <> drop 1 (show [s1, s2, s3])

ribToSexp :: MonadIO m => Rib -> m Sexp
ribToSexp rib = do (_,_,sexp) <- go 0 [] rib; pure sexp
  where
    go :: MonadIO m => Int -> [(IORef RibObj, Int)] -> Rib -> m (Int, [(IORef RibObj, Int)], Sexp)
    go i cycles (RibInt n) = pure (i, cycles, SexpInt n)
    go i cycles (RibRef r) = do
      RibObj r1 r2 r3 <- readRef r
      let initial_i = i
      (i, cycles, v1) <- recurse i cycles r1
      (i, cycles, v2) <- recurse i cycles r2
      (i, cycles, v3) <- recurse i cycles r3
      pure (i, cycles, SexpLst initial_i v1 v2 v3)

    recurse :: MonadIO m => Int -> [(IORef RibObj, Int)] -> Rib -> m (Int, [(IORef RibObj, Int)], Sexp)
    recurse i cycles (RibInt n) = pure (i, cycles, SexpInt n)
    recurse i cycles r@(RibRef ref) = do
      case lookup ref cycles of
        Nothing -> go (i + 1) ((ref,i):cycles) r
        Just cycleId -> pure (i, cycles, SexpLoop cycleId)

-- Convert the data rib to JSON
-- For decoding instructions, see ribInstructionToJson
ribDataToJson :: Rib -> SIO Aeson.Value
ribDataToJson (RibInt n) = pure $ Aeson.Number (fromIntegral n)
ribDataToJson o@(RibRef r) = do
  RibObj v1 v2 tag <- readRef r
  case tag of
    -- Pair
    RibInt 0 -> do
      lst <- decodeList v1 v2
      pure $ Aeson.object ["list" .= lst]

    -- Procedure
    RibInt 1 -> do
      (arity, codeVals, env) <- decodeProc v1 v2
      pure $ Aeson.object ["arity" .= arity, "env" .= env, "instructions" .= codeVals]

    -- Symbol
    RibInt 2 -> do
      val  <- ribDataToJson v1
      name <- ribDataToJson v2
      pure $ addField name "_value" val

    -- String
    RibInt 3 -> do
      (len, chars) <- decodeString v1 v2
      pure $ Aeson.object ["string" .= chars, "length" .= len]

    -- Vector
    RibInt 4 -> do
      (len, lst) <- decodeVector v1 v2
      pure $ Aeson.object ["vector" .= lst, "length" .= len]

    -- Special value
    RibInt 5 -> do
      st <- get
      if ribFalse == o
        then pure $ Aeson.String "#(false)#"
      else if ribTrue == o
        then pure $ Aeson.String "#(true)#"
      else if ribNil == o
        then pure $ Aeson.String "#(nil)#"
      else pure $ Aeson.String "#()#"

    -- Unknown tag
    RibInt n -> do
      u1 <- ribDataToJson v1
      u2 <- ribDataToJson v2
      pure $ Aeson.object ["field0" .= u1, "field1" .= u2, "tag" .= n]

    -- Unknown object
    RibRef r -> do
      RibObj t1 t2 t3 <- readRef r
      a  <- ribDataToJson v1
      b  <- ribDataToJson v2
      u1 <- ribDataToJson t1
      u2 <- ribDataToJson t2
      u3 <- ribDataToJson t3
      pure $ Aeson.object ["field0" .= u1, "field1" .= u2, "tag" .= Aeson.object ["tag0" .= u1, "tag1" .= u2, "tag2" .= u3]]

  where
    -- FIXME: Partial function but works for now
    addField :: Aeson.Value -> Text -> Aeson.Value -> Aeson.Value
    addField (Aeson.Object obj) key val = Aeson.Object $ Map.insert key val obj

ribInstructionToJson :: Rib -> SIO [Aeson.Value]
ribInstructionToJson (RibInt n) = pure [] -- pure [Aeson.Number (fromIntegral n)]
ribInstructionToJson o@(RibRef r) = do
  RibObj tag v2 v3 <- readRef r
  case tag of
    RibInt 0 -> do
      case v3 of
        -- Jump
        RibInt 0 -> pure [Aeson.String "jump"]

        -- Call
        -- TODO: Make encoding more compact
        v3' -> do
          instr <- case v2 of
            RibInt n -> pure $ Aeson.String ("call: " <> pack (show n))
            r -> do
              obj <- ribDataToJson r
              pure $ Aeson.object ["call" .= obj]
          rest <- ribInstructionToJson v3'
          pure $ instr : rest

    -- Set
    RibInt 1 -> do
      instr <- case v2 of
            RibInt n -> pure $ Aeson.object ["set" .= n]
            o -> do
              obj <- ribDataToJson o
              pure $ Aeson.object ["set" .= obj]
      rest <- ribInstructionToJson v3
      pure $ instr : rest

    -- Get
    RibInt 2 -> do
      instr <- case v2 of
            RibInt n -> pure $ Aeson.object ["get" .= n]
            o -> do
              obj <- ribDataToJson o
              pure $ Aeson.object ["get" .= obj]
      rest <- ribInstructionToJson v3
      pure $ instr : rest

    -- Const
    RibInt 3 -> do
      instr <- case v2 of
        RibInt n -> pure $ Aeson.String ("push: " <> pack (show n))
        r -> do
          obj <- ribDataToJson r
          pure $ Aeson.object ["push" .= obj]
      rest <- ribInstructionToJson v3
      pure $ instr : rest

    -- If
    RibInt 4 -> do
      let instr = Aeson.String "if"
      rest <- ribInstructionToJson v3
      pure $ instr : rest

    -- Halt
    RibInt 5 -> do
      let instr = Aeson.String "halt"
      rest <- ribInstructionToJson v3
      pure $ instr : rest

    -- Unknown instruction tag
    RibInt n -> do
      val <- ribDataToJson v2
      let instr = Aeson.object ["instruction-code" .= n, "value" .= val]
      rest <- ribInstructionToJson v3
      pure $ instr : rest

    -- Misencoded instructions or data passing as an instruction
    tag -> do
      instr <- ribDataToJson o
      rest <- ribInstructionToJson v3
      pure $ instr : rest

decodeList :: Rib -> Rib -> SIO [Aeson.Value]
decodeList car cdr = do
  carValue  <- ribDataToJson car
  cdrValues <- case cdr of
                RibInt n -> do
                  -- Unexpected RibInt, improper list?
                  v <- ribDataToJson cdr
                  pure [v]

                RibRef r -> do
                  RibObj cadr cddr cdrTag <- readRef r
                  case cdrTag of
                    RibInt 5 -> pure [] -- Nil!
                    RibInt 0 -> decodeList cadr cddr -- -- Continuing
                    RibInt n -> error $ "Invalid Rib list. Unexpected tag: " <> show n
                    -- Checking if it's the primordial continuation at the bottom of the stack
                    RibRef r -> do
                      RibObj v1 v2 v3 <- readRef r
                      case (v1, v2, v3) of
                        (RibInt 5, RibInt 0, RibInt 0) -> pure ["##END_OF_STACK##"]
                        _ -> error "Invalid Rib list. Tag can't be an object."
  pure $ carValue : cdrValues

decodeVector :: Rib -> Rib -> SIO (Int, [Aeson.Value]) -- Length and elements
decodeVector elements len = do
  -- Length est bien un entier?
  case len of
    RibInt n -> do
      -- Les éléments sont bien un objet?
      case elements of
        RibInt i -> error $ "Vector elems is not a list. Tag: " <> show i
        RibRef r -> do
          RibObj v1 v2 elemsTag <- readRef r
          -- Cet objet est bien une pair ou la liste vide?
          case elemsTag of
            -- Pair
            RibInt 0 -> (n,) <$> decodeList v1 v2

            -- Liste vide
            RibInt 5 -> pure (0, [])

            RibInt n -> error $ "Vector elems is not a list. Tag: " <> show n
            RibRef {} -> error "Vector elems is not a list. Its tag is an object."

    RibRef {} -> error "Vector length is not an int"

decodeString :: Rib -> Rib -> SIO (Int, String)
decodeString elems len = do
  (len, vals) <- decodeVector elems len
  let toChar = \case
        Aeson.Number n ->
          case floatingOrInteger n of
            Left _ -> error "String cannot contain float."
            Right c -> pure $ chr c
        _ -> error "String contains non-characters."
  chars <- mapM toChar vals
  pure (len, chars)

decodeProc :: Rib -> Rib -> SIO (Int, [Aeson.Value], [Aeson.Value])
decodeProc code env = do
  (arity, codeVals) <- case code of
    RibInt n -> pure (-1, [])
    RibRef r -> do
      RibObj arity _unused codePtr <- readRef r
      case arity of
        RibInt arity -> do
          codeVals <- ribInstructionToJson codePtr
          pure (arity, codeVals)
        _ -> error "Proc code arity is not an number."

  stack <- getStack
  env <- if stack == env
          then pure [Aeson.String "stack"]
        --  else if symbolTableRef st == env
        --   then pure [Aeson.String "symbol table"]
         else case env of
          RibInt i -> pure []
          RibRef r -> do
            RibObj car cdr lstTag <- readRef r
            -- Cet objet est bien une pair ou la liste vide?
            case lstTag of
              -- Pair
              RibInt 0 -> decodeList car cdr

              RibInt 5 -> pure [] -- Liste vide
              RibInt n -> error $ "Proc env is not a list. Tag: " <> show n
              RibRef {} -> error "Proc env is not a list. Its tag is an object."

  pure (arity, codeVals, env)

printState :: SIO ()
printState = do
  liftIO $ putStrLn "Stack:"
  liftIO . print =<< ribToSexp =<< getStack
