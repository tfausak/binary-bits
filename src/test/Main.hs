{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Main ( main ) where

import Data.Bits ((.|.))
import Test.QuickCheck ((==>))

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BB
import qualified Data.Binary.Bits.Get as BB
import qualified Data.Binary.Bits.Put as BB
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Word as Word
import qualified Foreign
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QC

main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Internal test functions" $ do
      Hspec.it "prop_bitreq" $ QC.property prop_bitreq

  Hspec.describe "Custom test cases" $ do
      Hspec.it "prop_composite_case" $ QC.property prop_composite_case

  Hspec.describe "getByteString" $ do
      Hspec.it "prop_getByteString_negative" $ QC.property prop_getByteString_negative

  Hspec.describe "getLazyByteString" $ do
      Hspec.it "getLazyByteString == getByteString" $ QC.property
                     prop_getLazyByteString_equal_to_ByteString
      Hspec.it "getLazyByteString == getByteString (with shift)" $ QC.property
                     prop_getLazyByteString_equal_to_ByteString2

  Hspec.describe "isEmpty" $ do
      Hspec.it "prop_isEmptyOfEmptyEmpty" $ QC.property prop_isEmptyOfEmptyEmpty
      Hspec.it "prop_isEmptyOfNonEmptyEmpty" $ QC.property prop_isEmptyOfNonEmptyEmpty
      Hspec.it "prop_isEmptyOfConsumedEmpty" $ QC.property prop_isEmptyOfConsumedEmpty
      Hspec.it "prop_isEmptyOfNotConsumedNotEmpty" $ QC.property prop_isEmptyOfNotConsumedNotEmpty

  Hspec.describe "Fail" $ do
      Hspec.it "monadic fail" $ QC.property prop_fail

  Hspec.describe "prop_bitput_with_get_from_binary" $ do
      Hspec.it "Word8" $ QC.property  (prop_bitput_with_get_from_binary :: W [Word.Word8]  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_bitput_with_get_from_binary :: W [Word.Word16] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_bitput_with_get_from_binary :: W [Word.Word32] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_bitput_with_get_from_binary :: W [Word.Word64] -> QC.Property)

  Hspec.describe "prop_bitget_with_put_from_binary" $ do
      Hspec.it "Word8" $ QC.property  (prop_bitget_with_put_from_binary :: W [Word.Word8]  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_bitget_with_put_from_binary :: W [Word.Word16] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_bitget_with_put_from_binary :: W [Word.Word32] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_bitget_with_put_from_binary :: W [Word.Word64] -> QC.Property)

  Hspec.describe "prop_compare_put_with_naive" $ do
      Hspec.it "Word8" $ QC.property  (prop_compare_put_with_naive :: W [Word.Word8]  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_compare_put_with_naive :: W [Word.Word16] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_compare_put_with_naive :: W [Word.Word32] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_compare_put_with_naive :: W [Word.Word64] -> QC.Property)

  Hspec.describe "prop_compare_get_with_naive" $ do
      Hspec.it "Word8" $ QC.property  (prop_compare_get_with_naive:: W [Word.Word8]  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_compare_get_with_naive:: W [Word.Word16] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_compare_get_with_naive:: W [Word.Word32] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_compare_get_with_naive:: W [Word.Word64] -> QC.Property)

  Hspec.describe "prop_put_with_bitreq" $ do
      Hspec.it "Word8" $ QC.property  (prop_putget_with_bitreq :: W Word.Word8  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_putget_with_bitreq :: W Word.Word16 -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_putget_with_bitreq :: W Word.Word32 -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_putget_with_bitreq :: W Word.Word64 -> QC.Property)

  Hspec.describe "prop_putget_list_simple" $ do
      Hspec.it "Bool" $ QC.property  (prop_putget_list_simple :: W [Bool]   -> QC.Property)
      Hspec.it "Word8" $ QC.property (prop_putget_list_simple :: W [Word.Word8]  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_putget_list_simple :: W [Word.Word16] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_putget_list_simple :: W [Word.Word32] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_putget_list_simple :: W [Word.Word64] -> QC.Property)

  Hspec.describe "prop_putget_applicative_with_bitreq" $ do
      Hspec.it "Word8" $ QC.property (prop_putget_applicative_with_bitreq :: W [(Word.Word8,Word.Word8,Word.Word8)]  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_putget_applicative_with_bitreq :: W [(Word.Word16,Word.Word16,Word.Word16)] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_putget_applicative_with_bitreq :: W [(Word.Word32,Word.Word32,Word.Word32)] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_putget_applicative_with_bitreq :: W [(Word.Word64,Word.Word64,Word.Word64)] -> QC.Property)

  Hspec.describe "prop_putget_list_with_bitreq" $ do
      Hspec.it "Word8" $ QC.property  (prop_putget_list_with_bitreq :: W [Word.Word8]  -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_putget_list_with_bitreq :: W [Word.Word16] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_putget_list_with_bitreq :: W [Word.Word32] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_putget_list_with_bitreq :: W [Word.Word64] -> QC.Property)
  Hspec.describe "prop_bitget_bytestring_interspersed" $ do
      Hspec.it "Word8" $ QC.property  (prop_bitget_bytestring_interspersed :: W Word.Word8  -> [B.ByteString] -> QC.Property)
      Hspec.it "Word16" $ QC.property (prop_bitget_bytestring_interspersed :: W Word.Word16 -> [B.ByteString] -> QC.Property)
      Hspec.it "Word32" $ QC.property (prop_bitget_bytestring_interspersed :: W Word.Word32 -> [B.ByteString] -> QC.Property)
      Hspec.it "Word64" $ QC.property (prop_bitget_bytestring_interspersed :: W Word.Word64 -> [B.ByteString] -> QC.Property)
  Hspec.describe "Simulate programs" $ do
      Hspec.it "primitive" $ QC.property prop_primitive
      Hspec.it "many primitives in sequence" $ QC.property prop_program

prop_isEmptyOfEmptyEmpty :: Bool
prop_isEmptyOfEmptyEmpty = Binary.runGet (BB.runBitGet BB.isEmpty) L.empty

prop_isEmptyOfNonEmptyEmpty :: L.ByteString -> QC.Property
prop_isEmptyOfNonEmptyEmpty bs =
  not (L.null bs) ==> not (Binary.runGet (BB.runBitGet BB.isEmpty) bs)

prop_isEmptyOfConsumedEmpty :: L.ByteString -> QC.Property
prop_isEmptyOfConsumedEmpty bs =
  not (L.null bs) ==>
    Binary.runGet (BB.runBitGet (BB.getByteString n >> BB.isEmpty)) bs
    where n = fromIntegral $ L.length bs

prop_isEmptyOfNotConsumedNotEmpty :: L.ByteString -> Int -> QC.Property
prop_isEmptyOfNotConsumedNotEmpty bs n =
  (fromIntegral n) < L.length bs && not (L.null bs) ==>
    not (Binary.runGet (BB.runBitGet (BB.getByteString n >> BB.isEmpty)) bs)

prop_getLazyByteString_equal_to_ByteString :: L.ByteString -> Int -> QC.Property
prop_getLazyByteString_equal_to_ByteString bs n =
  (fromIntegral n) <= L.length bs ==>
    Binary.runGet (BB.runBitGet (BB.getLazyByteString (fromIntegral n))) bs ==
            (L.fromChunks . (:[]) $ Binary.runGet (BB.runBitGet (BB.getByteString n)) bs)

prop_getLazyByteString_equal_to_ByteString2 :: L.ByteString -> Int -> QC.Property
prop_getLazyByteString_equal_to_ByteString2 bs n =
  (L.length bs > 1) && (fromIntegral n) < L.length bs ==>
    Binary.runGet (BB.runBitGet (BB.getWord8 2 >> BB.getLazyByteString (fromIntegral n))) bs ==
            (L.fromChunks . (:[]) $ Binary.runGet (BB.runBitGet (BB.getWord8 2 >> BB.getByteString n)) bs)

prop_getByteString_negative :: Int -> QC.Property
prop_getByteString_negative n =
  n < 1 ==>
    Binary.runGet (BB.runBitGet (BB.getByteString n)) L.empty == B.empty

prop_putget_with_bitreq :: (BB.BinaryBit a, Num a, Bits.Bits a, Ord a) => W a -> QC.Property
prop_putget_with_bitreq (W w) = QC.property $
  -- write all words with as many bits as it's required
  let p = BB.putBits (bitreq w) w
      g = BB.getBits (bitreq w)
      lbs = Binary.runPut (BB.runBitPut p)
      w' = Binary.runGet (BB.runBitGet g) lbs
  in w == w'

-- | Write a list of items. Each item is written with the maximum amount of
-- bits, i.e. 8 for Word8, 16 for Word16, etc.
prop_putget_list_simple :: (BB.BinaryBit a, Eq a, Foreign.Storable a) => W [a] -> QC.Property
prop_putget_list_simple (W ws) = QC.property $
  let s = Foreign.sizeOf (head ws) * 8
      p = mapM_ (BB.putBits s) ws
      g = mapM  (const (BB.getBits s)) ws
      lbs = Binary.runPut (BB.runBitPut p)
      ws' = Binary.runGet (BB.runBitGet g) lbs
  in ws == ws'

-- | Write a list of items. Each item is written with exactly as many bits
-- as required. Then read it back.
prop_putget_list_with_bitreq :: (BB.BinaryBit a, Num a, Bits.Bits a, Ord a) => W [a] -> QC.Property
prop_putget_list_with_bitreq (W ws) = QC.property $
  -- write all words with as many bits as it's required
  let p = mapM_ (\v -> BB.putBits (bitreq v) v) ws
      g = mapM BB.getBits bitlist
      lbs = Binary.runPut (BB.runBitPut p)
      ws' = Binary.runGet (BB.runBitGet g) lbs
  in ws == ws'
  where
    bitlist = map bitreq ws

prop_putget_applicative_with_bitreq :: (BB.BinaryBit a, Num a, Bits.Bits a, Ord a) => W [(a,a,a)] -> QC.Property
prop_putget_applicative_with_bitreq (W ts) = QC.property $
  let p = mapM_ (\(a,b,c) -> do BB.putBits (bitreq a) a
                                BB.putBits (bitreq b) b
                                BB.putBits (bitreq c) c) ts
      g = mapM (\(a,b,c) -> (,,) <$> BB.getBits a <*> BB.getBits b <*> BB.getBits c) bitlist
      lbs = Binary.runPut (BB.runBitPut p)
      ts' = Binary.runGet (BB.runBitGet g) lbs
  in ts == ts'
  where
    bitlist = map (\(a,b,c) -> (bitreq a, bitreq b, bitreq c)) ts

-- | Write bits using this library, and read them back using the binary
-- library.
prop_bitput_with_get_from_binary :: (BB.BinaryBit a, Binary.Binary a, Foreign.Storable a, Eq a) => W [a] -> QC.Property
prop_bitput_with_get_from_binary (W ws) = QC.property $
  let s = Foreign.sizeOf (head ws) * 8
      p = mapM_ (BB.putBits s) ws
      g = mapM (const Binary.get) ws
      lbs = Binary.runPut (BB.runBitPut p)
      ws' = Binary.runGet g lbs
  in ws == ws'

-- | Write bits using the binary library, and read them back using this
-- library.
prop_bitget_with_put_from_binary :: (BB.BinaryBit a, Binary.Binary a, Foreign.Storable a, Eq a) => W [a] -> QC.Property
prop_bitget_with_put_from_binary (W ws) = QC.property $
  let s = Foreign.sizeOf (head ws) * 8
      p = mapM_ Binary.put ws
      g = mapM (const (BB.getBits s)) ws
      lbs = Binary.runPut p
      ws' = Binary.runGet (BB.runBitGet g) lbs
  in ws == ws'

-- | Write each 'ByteString' with a variable sized value as a separator.
prop_bitget_bytestring_interspersed :: (BB.BinaryBit a, Binary.Binary a, Num a, Ord a, Bits.Bits a) => W a -> [B.ByteString] -> QC.Property
prop_bitget_bytestring_interspersed (W ws) bss = QC.property $
  let p = mapM_ (\bs -> BB.putBits (bitreq ws) ws >> BB.putByteString bs) bss
      g = mapM (\bs -> (,) <$> BB.getBits (bitreq ws) <*> BB.getByteString (B.length bs)) bss
      lbs = Binary.runPut (BB.runBitPut p)
      r = Binary.runGet (BB.runBitGet g) lbs
  in map ((,) ws) bss == r

-- | Test failing.
prop_fail :: L.ByteString -> String -> QC.Property
prop_fail lbs errMsg0 = QC.forAll (QC.choose (0, 8 * L.length lbs)) $ \len ->
  let (bytes,bits) = len `divMod` 8
      expectedBytesConsumed
        | bits == 0 = bytes
        | otherwise = bytes + 1
      p = do _ <- BB.getByteString (fromIntegral bytes)
             _ <- BB.getBits (fromIntegral bits) :: BB.BitGet Word.Word8
             fail errMsg0
      r = Binary.runGetIncremental (BB.runBitGet p) `Binary.pushChunks` lbs
  in case r of
       Binary.Fail remainingBS pos errMsg ->
         and [ L.fromChunks [remainingBS] == L.drop expectedBytesConsumed lbs
             , pos == expectedBytesConsumed
             , errMsg == errMsg0
             ]
       _ -> False

-- | number of bits required to write @v@
bitreq :: (Num b, Num a, Bits.Bits a, Ord a) => a -> b
bitreq v = fromIntegral . head $ [ req | (req, top) <- bittable, v <= top ]

bittable :: (Bits.Bits a, Num a) => [(Integer, a)]
bittable = [ (fromIntegral x, (1 `Bits.shiftL` x) - 1) | x <- [1..64] ]

prop_bitreq :: W Word.Word64 -> QC.Property
prop_bitreq (W w) = QC.property $
  ( w == 0 && bitreq w == (1 :: Integer) )
    || bitreq w == bitreq (w `Bits.shiftR` 1) + (1 :: Integer)

prop_composite_case :: Bool -> W Word.Word16 -> QC.Property
prop_composite_case b (W w) = w < 0x8000 ==>
  let p = do BB.putBool b
             BB.putWord16be 15 w
      g = do v <- BB.getBool
             case v of
              True -> BB.getWord16be 15
              False -> do
                msb <- BB.getWord8 7
                lsb <- BB.getWord8 8
                return ((fromIntegral msb `Bits.shiftL` 8) .|. fromIntegral lsb)
      lbs = Binary.runPut (BB.runBitPut p)
      w' = Binary.runGet (BB.runBitGet g) lbs
  in w == w'

prop_compare_put_with_naive :: (Bits.Bits a, BB.BinaryBit a, Ord a, Num a) => W [a] -> QC.Property
prop_compare_put_with_naive (W ws) = QC.property $
  let pn = mapM_ (\v -> naive_put (bitreq v) v) ws
      p  = mapM_ (\v -> BB.putBits   (bitreq v) v) ws
      lbs_n = Binary.runPut (BB.runBitPut pn)
      lbs   = Binary.runPut (BB.runBitPut p)
  in lbs_n == lbs

prop_compare_get_with_naive :: (Bits.Bits a, BB.BinaryBit a, Ord a, Num a) => W [a] -> QC.Property
prop_compare_get_with_naive (W ws) = QC.property $
  let gn = mapM  (\v -> naive_get (bitreq v)) ws
      g  = mapM  (\v -> BB.getBits   (bitreq v)) ws
      p  = mapM_ (\v -> naive_put (bitreq v) v) ws
      lbs = Binary.runPut (BB.runBitPut p)
      rn = Binary.runGet (BB.runBitGet gn) lbs
      r  = Binary.runGet (BB.runBitGet g ) lbs
      -- we must help our compiler to resolve the types of 'gn' and 'g'
      _types = rn == ws && r == ws
  in rn == r

-- | Write one bit at a time until the full word has been written
naive_put :: (Bits.Bits a) => Int -> a -> BB.BitPut ()
naive_put n w = mapM_ (\b -> BB.putBool (Bits.testBit w b)) [n-1,n-2..0]

-- | Read one bit at a time until we've reconstructed the whole word
naive_get :: (Bits.Bits a, Num a) => Int -> BB.BitGet a
naive_get n0 =
  let loop 0 acc = return acc
      loop n acc = do
        b <- BB.getBool
        case b of
          False -> loop (n-1) (acc `Bits.shiftL` 1)
          True  -> loop (n-1) ((acc `Bits.shiftL` 1) + 1)
  in loop n0 0

shrinker :: (Num a, Ord a, Bits.Bits a) => a -> [a]
shrinker 0 = []
shrinker w = [ w `Bits.shiftR` 1 -- try to make everything roughly half size
             ] ++ [ w' -- flip bits to zero, left->right
                  | m <- [n, n-1..1]
                  , let w' = w `Bits.clearBit` m
                  , w /= w'
                  ] ++ [w-1] -- just make it a little smaller
  where
    n = bitreq w

data W a = W { unW :: a } deriving (Show, Eq, Ord)

arbitraryW :: (QC.Arbitrary (W a)) => QC.Gen a
arbitraryW = unW <$> QC.arbitrary

shrinkW :: (QC.Arbitrary (W a)) => a -> [a]
shrinkW x = unW <$> QC.shrink (W x)

instance QC.Arbitrary (W Bool) where
    arbitrary       = W <$> QC.arbitrary
    shrink          = map W <$> QC.shrink . unW

instance QC.Arbitrary (W Word.Word8) where
    arbitrary       = W <$> QC.choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance QC.Arbitrary (W Word.Word16) where
    arbitrary       = W <$> QC.choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance QC.Arbitrary (W Word.Word32) where
    arbitrary       = W <$> QC.choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance QC.Arbitrary (W Word.Word64) where
    arbitrary       = W <$> QC.choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance QC.Arbitrary B.ByteString where
    arbitrary       = B.pack <$> QC.arbitrary
    shrink bs       = B.pack <$> QC.shrink (B.unpack bs)

instance QC.Arbitrary L.ByteString where
    arbitrary       = L.fromChunks <$> QC.arbitrary
    shrink bs       = L.fromChunks <$> QC.shrink (L.toChunks bs)

instance (QC.Arbitrary (W a)) => QC.Arbitrary (W [a]) where
    arbitrary       = W . map unW <$> QC.arbitrary
    shrink          = map (W . map unW) <$> mapM QC.shrink . map W . unW

instance (QC.Arbitrary (W a), QC.Arbitrary (W b)) => QC.Arbitrary (W (a,b)) where
    arbitrary        = (W .) . (,) <$> arbitraryW <*> arbitraryW
    shrink (W (a,b)) = (W .) . (,) <$> shrinkW a <*> shrinkW b

instance (QC.Arbitrary (W a), QC.Arbitrary (W b), QC.Arbitrary (W c)) => QC.Arbitrary (W (a,b,c)) where
    arbitrary          = ((W .) .) . (,,) <$> arbitraryW <*> arbitraryW <*> arbitraryW
    shrink (W (a,b,c)) = ((W .) .) . (,,) <$> shrinkW a <*> shrinkW b <*> shrinkW c

data Primitive
  = Bool Bool
  | W8  Int Word.Word8
  | W16 Int Word.Word16
  | W32 Int Word.Word32
  | W64 Int Word.Word64
  | BS  Int B.ByteString
  | LBS Int L.ByteString
  | IsEmpty
  deriving (Eq, Show)

type Program = [Primitive]

instance QC.Arbitrary Primitive where
  arbitrary = do
    let gen c = do
          let (maxBits, _) = (\w -> (Bits.finiteBitSize w, c undefined w)) undefined
          bits <- QC.choose (0, maxBits)
          n <- QC.choose (0, fromIntegral (2^bits-1 :: Integer))
          return (c bits n)
    QC.oneof
      [ Bool <$> QC.arbitrary
      , gen W8
      , gen W16
      , gen W32
      , gen W64
      , do n <- QC.choose (0,10)
           cs <- QC.vector n
           return (BS n (B.pack cs))
      , do n <- QC.choose (0,10)
           cs <- QC.vector n
           return (LBS n (L.pack cs))
      , return IsEmpty
      ]
  shrink p =
    let snk c x = map (\x' -> c (bitreq x') x') (shrinker x) in
    case p of
      Bool b -> if b then [Bool False] else []
      W8 _ x -> snk W8 x
      W16 _ x -> snk W16 x
      W32 _ x -> snk W32 x
      W64 _ x -> snk W64 x
      BS _ bs -> let ws = B.unpack bs in map (\ws' -> BS (length ws') (B.pack ws')) (QC.shrink ws)
      LBS _ lbs -> let ws = L.unpack lbs in map (\ws' -> LBS (length ws') (L.pack ws')) (QC.shrink ws)
      IsEmpty -> []

prop_primitive :: Primitive -> QC.Property
prop_primitive prim = QC.property $
  let p = putPrimitive prim
      g = getPrimitive prim
      lbs = Binary.runPut (BB.runBitPut p)
      r = Binary.runGet (BB.runBitGet g) lbs
  in r == prim

prop_program :: Program -> QC.Property
prop_program program = QC.property $
  let p = mapM_ putPrimitive program
      g = verifyProgram (8 * fromIntegral (L.length lbs)) program
      lbs = Binary.runPut (BB.runBitPut p)
      r = Binary.runGet (BB.runBitGet g) lbs
  in r

putPrimitive :: Primitive -> BB.BitPut ()
putPrimitive p =
  case p of
    Bool b -> BB.putBool b
    W8 n x -> BB.putWord8 n x
    W16 n x -> BB.putWord16be n x
    W32 n x -> BB.putWord32be n x
    W64 n x -> BB.putWord64be n x
    BS _ bs -> BB.putByteString bs
    LBS _ lbs -> mapM_ BB.putByteString (L.toChunks lbs)
    IsEmpty -> return ()

getPrimitive :: Primitive -> BB.BitGet Primitive
getPrimitive p =
  case p of
    Bool _ -> Bool <$> BB.getBool
    W8 n _ -> W8 n <$> BB.getWord8 n
    W16 n _ -> W16 n <$> BB.getWord16be n
    W32 n _ -> W32 n <$> BB.getWord32be n
    W64 n _ -> W64 n <$> BB.getWord64be n
    BS n _ -> BS n <$> BB.getByteString n
    LBS n _ -> LBS n <$> BB.getLazyByteString n
    IsEmpty -> BB.isEmpty >> return IsEmpty


verifyProgram :: Int -> Program -> BB.BitGet Bool
verifyProgram totalLength ps0 = go 0 ps0
  where
    go _ [] = return True
    go pos (p:ps) =
      case p of
        Bool x -> check x BB.getBool >> go (pos+1) ps
        W8 n x ->  check x (BB.getWord8 n) >> go (pos+n) ps
        W16 n x -> check x (BB.getWord16be n) >> go (pos+n) ps
        W32 n x -> check x (BB.getWord32be n) >> go (pos+n) ps
        W64 n x -> check x (BB.getWord64be n) >> go (pos+n) ps
        BS n x -> check x (BB.getByteString n) >> go (pos+(8*n)) ps
        LBS n x -> check x (BB.getLazyByteString n) >> go (pos+(8*n)) ps
        IsEmpty -> do
          let expected = pos == totalLength
          actual <- BB.isEmpty
          if expected == actual
            then go pos ps
            else error $ "isEmpty returned wrong value, expected "
                          ++ show expected ++ " but got " ++ show actual
    check x g = do
      y <- g
      if x == y
        then return ()
        else error $ "Roundtrip error: Expected "
                     ++ show x ++ " but got " ++ show y
