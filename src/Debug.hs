{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debug where

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
import VM

-- Debugging functions

printRib :: Rib -> ReaderIO State ()
printRib r = liftIO . BS.putStrLn . Aeson.encodePretty =<< ribDataToJson r

printInstrRib :: Rib -> ReaderIO State ()
printInstrRib r = liftIO . BS.putStrLn . Aeson.encodePretty =<< ribInstructionToJson r

printRibList :: Rib -> ReaderIO State ()
printRibList (RibObj car cdr _) = liftIO . BS.putStrLn . Aeson.encodePretty =<< decodeList car cdr
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

data Sexp = SexpName String | SexpInt Int | SexpLst Sexp Sexp Sexp
  deriving (Eq)

-- Permet d'écrire `n` plutôt que `SexpInt n` quand `n` est un
-- entier litéral.
instance Num Sexp where
  fromInteger = SexpInt . fromInteger

instance Show Sexp where
  show (SexpName n) = n
  show (SexpInt n) = show n
  show (SexpLst s1 s2 s3) = show [s1, s2, s3]

ribToSexp :: Rib -> ReaderIO State Sexp
ribToSexp (RibInt n) = pure (SexpInt n)
ribToSexp (RibObj r1 r2 r3) = do
  v1 <- recurse r1
  v2 <- recurse r2
  v3 <- recurse r3
  pure $ SexpLst v1 v2 v3
  where
    recurse r = do
      st <- get
      if stackRef st == r
        then pure $ SexpName "stack"
      else if symbolTableRef st == r
        then pure $ SexpName "symbol table"
      else readRef r >>= ribToSexp

-- Convert the data rib to JSON
-- For decoding instructions, see ribInstructionToJson
ribDataToJson :: Rib -> ReaderIO State Aeson.Value
ribDataToJson (RibInt n) = pure $ Aeson.Number (fromIntegral n)
ribDataToJson o@(RibObj v1 v2 tag) = do
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

    -- Unknown tag
    RibInt n -> do
      u1 <- ribDataToJson =<< readRef v1
      u2 <- ribDataToJson =<< readRef v2
      pure $ Aeson.object ["field0" .= u1, "field1" .= u2, "tag" .= n]

    -- Unknown object
    RibObj t1 t2 t3 -> do
      a  <- ribDataToJson =<< readRef v1
      b  <- ribDataToJson =<< readRef v2
      u1 <- ribDataToJson =<< readRef t1
      u2 <- ribDataToJson =<< readRef t2
      u3 <- ribDataToJson =<< readRef t3
      pure $ Aeson.object ["field0" .= u1, "field1" .= u2, "tag" .= Aeson.object ["tag0" .= u1, "tag1" .= u2, "tag2" .= u3]]

  where
    -- FIXME: Partial function but works for now
    addField :: Aeson.Value -> Text -> Aeson.Value -> Aeson.Value
    addField (Aeson.Object obj) key val = Aeson.Object $ Map.insert key val obj

ribInstructionToJson :: Rib -> ReaderIO State [Aeson.Value]
ribInstructionToJson (RibInt n) = pure [] -- pure [Aeson.Number (fromIntegral n)]
ribInstructionToJson o@(RibObj tag v2 v3) = do
  tag <- readRef tag
  case tag of
    RibInt 0 -> do
      readRef v3 >>= \case
        -- Jump
        RibInt 0 -> pure [Aeson.String "jump"]

        -- Call
        -- TODO: Make encoding more compact
        v3' -> do
          val <- readRef v2
          instr <- case val of
            RibInt n -> pure $ Aeson.String ("call: " <> pack (show n))
            r -> do
              obj <- ribDataToJson r
              pure $ Aeson.object ["call" .= obj]
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
      val <- readRef v2
      instr <- case val of
        RibInt n -> pure $ Aeson.String ("push: " <> pack (show n))
        r -> do
          obj <- ribDataToJson r
          pure $ Aeson.object ["push" .= obj]
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    -- If
    RibInt 4 -> do
      let instr = Aeson.String "if"
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    -- Halt
    RibInt 5 -> do
      let instr = Aeson.String "halt"
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    -- Unknown instruction tag
    RibInt n -> do
      val <- readRef v2 >>= ribDataToJson
      let instr = Aeson.object ["instruction-code" .= n, "value" .= val]
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

    -- Misencoded instructions or data passing as an instruction
    tag -> do
      instr <- ribDataToJson o
      rest <- readRef v3 >>= ribInstructionToJson
      pure $ instr : rest

decodeList :: IORef Rib -> IORef Rib -> ReaderIO State [Aeson.Value]
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
                  -- Checking if it's the primordial continuation at the bottom of the stack
                  RibObj v1 v2 v3 -> do
                    a <- (,,) <$> readRef v1 <*> readRef v2 <*> readRef v3
                    case a of
                      (RibInt 5, RibInt 0, RibInt 0) -> pure ["##END_OF_STACK##"]
                      _ -> error "Invalid Rib list. Tag can't be an object."
  pure $ carValue : cdrValues

decodeVector :: IORef Rib -> IORef Rib -> ReaderIO State (Int, [Aeson.Value]) -- Length and elements
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

decodeString :: IORef Rib -> IORef Rib -> ReaderIO State (Int, String)
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

decodeProc :: IORef Rib -> IORef Rib -> ReaderIO State (Int, [Aeson.Value], [Aeson.Value])
decodeProc codeRef envRef = do
  (arity, codeVals) <- readRef codeRef >>= \case
    RibInt n -> pure (-1, [])
    RibObj arityRef _unused codePtr -> do
      readRef arityRef >>= \case
        RibInt arity -> do
          codeVals <- readRef codePtr >>= ribInstructionToJson
          pure (arity, codeVals)
        _ -> error "Proc code arity is not an number."

  st <- get
  env <- if stackRef st == envRef
          then pure [Aeson.String "stack"]
         else if symbolTableRef st == envRef
          then pure [Aeson.String "symbol table"]
         else readRef envRef >>= \case
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
