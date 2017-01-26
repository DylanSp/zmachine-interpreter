{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--put this in Cabal file

module Utilities where

--note: make this a module, exporting everything
--put test code in Test/ directory.

import Data.Bits
import Data.Word

newtype BitNumber = BitNumber Int

newtype BitSize = BitSize Int

newtype ByteAddress = ByteAddress Int deriving (Show)

newtype WordAddress = WordAddress Int

newtype ZWord = ZWord Word16 deriving (Eq, Ord, Num, Enum, Bits, Real, Integral, Show)

--ZWord size in bytes
zWordSize :: Int
zWordSize = 2

bit0 = BitNumber 0
bit1 = BitNumber 1
bit2 = BitNumber 2
bit3 = BitNumber 3
bit4 = BitNumber 4
bit5 = BitNumber 5
bit6 = BitNumber 6
bit7 = BitNumber 7
bit8 = BitNumber 8
bit9 = BitNumber 9
bit10 = BitNumber 10
bit11 = BitNumber 11
bit12 = BitNumber 12
bit13 = BitNumber 13
bit14 = BitNumber 14
bit15 = BitNumber 15

size1 = BitSize 1
size2 = BitSize 2
size3 = BitSize 3
size4 = BitSize 4
size5 = BitSize 5
size6 = BitSize 6
size7 = BitSize 7

fetchBit :: (Bits a) => BitNumber -> a -> Bool
fetchBit (BitNumber n) word = testBit word n

fetchBits :: (Bits a) => BitNumber -> BitSize -> a -> a
fetchBits (BitNumber high) (BitSize len) word = (word `shiftR` (high - len + 1)) .&. mask
    where mask = complement ((complement zeroBits) `shiftL` len)
          zeroBits = clearBit (bit 0) 0
          
--testFetch :: Bool
--testFetch = (fetchBits bit15 size4 0xBEEF) == 0xB 

isInRange :: ByteAddress -> Int -> Bool
isInRange (ByteAddress address) size = 
    0 <= address && address < size
    
isOutOfRange :: ByteAddress -> Int -> Bool
isOutOfRange address size = not $ isInRange address size

incByteAddrBy :: ByteAddress -> Int -> ByteAddress
incByteAddrBy (ByteAddress address) offset = ByteAddress (address + offset)

decByteAddrBy :: ByteAddress -> Int -> ByteAddress
decByteAddrBy address offset = incByteAddrBy address (0 - offset)

incWordAddrBy :: WordAddress -> Int -> WordAddress
incWordAddrBy (WordAddress address) offset = WordAddress (address + offset * zWordSize)

incWordAddr :: WordAddress -> WordAddress
incWordAddr address = incWordAddrBy address 1

dereferenceString :: [Word8] -> ByteAddress -> Word8
dereferenceString bytes addr@(ByteAddress address) 
    | isOutOfRange addr (length bytes)  = error $ "dereferenceString: Address out of range\n" ++ "addr: " ++ (show addr) ++ "\nlength bytes: " ++ (show $ length bytes)
    | otherwise                         = bytes !! address
    
addressOfHighByte :: WordAddress -> ByteAddress
addressOfHighByte (WordAddress address) = ByteAddress address

addressOfLowByte :: WordAddress -> ByteAddress
addressOfLowByte (WordAddress address) = ByteAddress (address + 1)

