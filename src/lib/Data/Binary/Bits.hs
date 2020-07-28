-- | Parse and write bits easily. Parsing can be done either in a monadic
-- style, or more efficiently, using the 'Applicative' style. Writing is
-- monadic style only. See "Data.Binary.Bits.Get" and "Data.Binary.Bits.Put",
-- respectively.
module Data.Binary.Bits ( BinaryBit(putBits, getBits) ) where

import qualified Data.Binary.Bits.Get as Get
import qualified Data.Binary.Bits.Put as Put
import qualified Data.Word as Word

class BinaryBit a where
  putBits :: Int -> a -> Put.BitPut ()
  getBits :: Int -> Get.BitGet a

instance BinaryBit Bool where
  putBits _ = Put.putBool
  getBits _ = Get.getBool

instance BinaryBit Word.Word8 where
  putBits = Put.putWord8
  getBits = Get.getWord8

instance BinaryBit Word.Word16 where
  putBits = Put.putWord16be
  getBits = Get.getWord16be

instance BinaryBit Word.Word32 where
  putBits = Put.putWord32be
  getBits = Get.getWord32be

instance BinaryBit Word.Word64 where
  putBits = Put.putWord64be
  getBits = Get.getWord64be
