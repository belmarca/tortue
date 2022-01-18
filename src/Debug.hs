{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debug where

import Data.Aeson ( KeyValue((.=)) )
import Data.Char ( chr, ord )
import Data.Text ( Text )
import Data.IORef ( IORef )
import Data.Scientific ( floatingOrInteger )
import GHC.IO ( unsafePerformIO )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vector

import Utils
import Rib

-- Debugging functions

printRib :: Rib -> IO ()
printRib r = BS.putStrLn . Aeson.encodePretty =<< ribDataToJson r

printInstrRib :: Rib -> IO ()
printInstrRib r = BS.putStrLn . Aeson.encodePretty =<< ribInstructionToJson r

printRibList :: Rib -> IO ()
printRibList (RibObj car cdr _) = BS.putStrLn . Aeson.encodePretty =<< decodeList car cdr
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

-- For type inference
i :: Int -> Int
i = id

showRib :: Rib -> IO String
showRib rib = fst <$> go 1 "" rib
  where
    go :: Int -> String -> Rib -> IO (String, Int)
    go counter prefix (RibInt n) = pure (prefix <> show n, counter)
    go counter prefix (RibObj v1 v2 v3) = do
      v1 <- readRef v1
      v2 <- readRef v2
      v3 <- readRef v3
      (s1, counter1) <- go (counter  + 3)  ("    " <> prefix) v1
      (s2, counter2) <- go (counter1 + 1)  ("    " <> prefix) v2
      (s3, counter3) <- go (counter2 + 1)  ("    " <> prefix) v3

      let str = prefix <> "Object:\n" -- <> show counter <> "]:\n"
             <> show (counter + 1) <> ": " <> prefix <> s1 <> "\n"
             <> show (counter + 2) <> ": " <> prefix <> s2 <> "\n"
             <> show (counter + 3) <> ": " <> prefix <> s3
      pure (str, counter3)

-- Convert the data rib to JSON
-- For decoding instructions, see ribInstructionToJson
ribDataToJson :: forall m. MonadIO m => Rib -> m Aeson.Value
ribDataToJson (RibInt n) = pure $ Aeson.Number (fromIntegral n)
ribDataToJson (RibObj v1 v2 tag) = do
  tag <- readRef tag
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
      val  <- ribDataToJson =<< readRef v1
      name <- ribDataToJson =<< readRef v2
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
    RibInt 5 -> pure $ Aeson.String "#()#"

    -- Unknown object
    _ -> error "Unknown object"

  where
    -- FIXME: Partial function but works for now
    addField :: Aeson.Value -> Text -> Aeson.Value -> Aeson.Value
    addField (Aeson.Object obj) key val = Aeson.Object $ Map.insert key val obj

ribInstructionToJson :: forall m. MonadIO m => Rib -> m [Aeson.Value]
ribInstructionToJson (RibInt n) = pure [] -- pure [Aeson.Number (fromIntegral n)]
ribInstructionToJson (RibObj tag v2 v3) = do
  tag <- readRef tag
  case tag of
    RibInt 0 -> do
      readRef v3 >>= \case
        -- Jump
        RibInt 0 -> pure $ [Aeson.String "jump"]

        -- Call
        v3' -> do
          let instr = Aeson.String "call"
          rest <- ribInstructionToJson v3'
          pure $ instr : rest

    -- Set
    RibInt 1 -> do
      let instr = Aeson.String "set"
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    -- Get
    RibInt 2 -> do
      let instr = Aeson.String "get"
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    -- Const
    RibInt 3 -> do
      obj <- readRef v2 >>= ribDataToJson
      let instr = Aeson.object ["push" .= obj]
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    -- If
    RibInt 4 -> do
      let instr = Aeson.String "if"
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    _ -> error "Unknown instruction"

decodeList :: MonadIO m => IORef Rib -> IORef Rib -> m [Aeson.Value]
decodeList car cdr = do
  carValue  <- ribDataToJson =<< readRef car
  cdrValue  <- readRef cdr
  cdrValues <- case cdrValue of
                RibInt n -> do
                  -- Unexpected RibInt, improper list?
                  v <- ribDataToJson cdrValue
                  pure [v]

                RibObj cadr cddr cdrTag -> readRef cdrTag >>= \case
                  RibInt 5 -> pure [] -- Nil!
                  RibInt 0 -> decodeList cadr cddr -- -- Continuing
                  RibInt n -> error $ "Invalid Rib list. Unexpected tag: " <> show n
                  RibObj {} -> error "Invalid Rib list. Tag can't be an object."
  pure $ carValue : cdrValues

decodeVector :: MonadIO m => IORef Rib -> IORef Rib -> m (Int, [Aeson.Value]) -- Length and elements
decodeVector elemsRef lengthRef = do
  len <- readRef lengthRef
  -- Length est bien un entier?
  case len of
    RibInt n -> do
      elems <- readRef elemsRef
      -- Les éléments sont bien un objet?
      case elems of
        RibInt i -> error $ "Vector elems is not a list. Tag: " <> show i
        RibObj v1 v2 elemsTag -> do
          lstTag <- readRef elemsTag
          -- Cet objet est bien une pair ou la liste vide?
          case lstTag of
            -- Pair
            RibInt 0 -> (n,) <$> decodeList v1 v2

            -- Liste vide
            RibInt 5 -> pure (0, [])

            RibInt n -> error $ "Vector elems is not a list. Tag: " <> show n
            RibObj {} -> error "Vector elems is not a list. Its tag is an object."

    RibObj {} -> error "Vector length is not an int"

decodeString :: MonadIO m => IORef Rib -> IORef Rib -> m (Int, String)
decodeString elemsRef lengthRef = do
  (len, vals) <- decodeVector elemsRef lengthRef
  let toChar = \case
        Aeson.Number n ->
          case floatingOrInteger n of
            Left _ -> error "String cannot contain float."
            Right c -> pure $ chr c
        _ -> error "String contains non-characters."
  chars <- mapM toChar vals
  pure (len, chars)

decodeProc :: MonadIO m => IORef Rib -> IORef Rib -> m (Int, [Aeson.Value], [Aeson.Value])
decodeProc codeRef envRef = do
  (arity, codeVals) <- readRef codeRef >>= \case
    RibInt n -> error "Proc code is not an object."
    RibObj arityRef _unused codePtr -> do
      readRef arityRef >>= \case
        RibInt arity -> do
          codeVals <- readRef codePtr >>= ribInstructionToJson
          pure (arity, codeVals)
        _ -> error "Proc code arity is not an number."

  env <- readRef envRef >>= \case
    RibInt i -> pure []
    RibObj car cdr lstTag ->
      -- Cet objet est bien une pair ou la liste vide?
      readRef lstTag >>= \case
        -- Pair
        RibInt 0 -> decodeList car cdr

        RibInt 5 -> pure [] -- Liste vide
        RibInt n -> error $ "Proc env is not a list. Tag: " <> show n
        RibObj {} -> error "Proc env is not a list. Its tag is an object."

  pure (arity, codeVals, env)
