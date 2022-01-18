{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Debug where

import Data.Aeson ( KeyValue((.=)) )
import Data.Char ( chr, ord )
import Data.IORef ( IORef )
import Data.Scientific ( floatingOrInteger )
import GHC.IO ( unsafePerformIO )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vector

import Utils
import VM

-- Debugging functions

printRib :: Rib -> IO ()
printRib r = BS.putStr . Aeson.encodePretty =<< ribDataToJson r

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
    RibInt 1 -> error "FIXME: Decode procedure"

    -- Symbol
    RibInt 2 -> do
      val  <- ribDataToJson =<< readRef v1
      name <- ribDataToJson =<< readRef v2
      pure $ Aeson.object ["symbol" .= name, "value" .= val]

    -- String
    RibInt 3 -> do
      (len, chars) <- decodeString v1 v2
      pure $ Aeson.object ["string" .= chars, "length" .= len]

    -- Vector
    RibInt 4 -> do
      (len, lst) <- decodeVector v1 v2
      pure $ Aeson.object ["vector" .= lst, "length" .= len]

    -- Special value
    RibInt 5 -> pure $ Aeson.String "()"

    -- Unknown object
    -- RibObj ir ir' ir2 ->
    _ -> error "Unknown object"
  where
    decodeList :: IORef Rib -> IORef Rib -> m [Aeson.Value]
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

    decodeVector :: IORef Rib -> IORef Rib -> m (Int, [Aeson.Value]) -- Length and elements
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
                RibInt 0 -> do
                  lst <- decodeList v1 v2
                  -- On retourne donc la longueur et les éléments
                  pure (n, lst)

                -- Liste vide
                RibInt 5 -> do
                  pure (0, [])

                RibInt n -> error $ "Vector elems is not a list. Tag: " <> show n
                RibObj {} -> error "Vector elems is not a list. Its tag is an object."

        RibObj {} -> error "Vector length is not an int"

    decodeString :: IORef Rib -> IORef Rib -> m (Int, String)
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

    -- TODO
    -- decodeProc :: IORef Rib -> IORef Rib -> m (Int, Aeson.Value)
    -- decodeProc codeRef envRef = do
    --   readRef codeRef >>= \case
    --     RibInt n -> error "Proc code is not an object."
    --     RibObj arityRef v2 codeTag -> do
    --       readRef arityRef >>= \case
    --         RibInt arity -> undefined
