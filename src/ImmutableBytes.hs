module ImmutableBytes 
( ImmutableBytes (..)
, size
, new
, readByte
, writeByte
) where 

import Utilities
import qualified Data.Map as Map
import Data.Word
import Data.Maybe (fromJust)

type IntMap = Map.Map Int

data ImmutableBytes = ImmutableBytes { originalBytes :: [Word8] 
                                     , edits :: (IntMap Word8) 
                                     }

size :: ImmutableBytes -> Int
size = length . originalBytes

new :: [Word8] -> ImmutableBytes
new bytes = ImmutableBytes {originalBytes = bytes, edits = Map.empty}

readByte :: ImmutableBytes -> ByteAddress -> Word8
readByte bytes addr@(ByteAddress address)
    | isOutOfRange addr (size bytes) = error "Address is out of range"
    | Map.member address edited      = fromJust $ Map.lookup address edited 
    | otherwise                      = (originalBytes bytes) !! address 
    where edited = edits bytes
    
writeByte :: ImmutableBytes -> ByteAddress -> Word8 -> ImmutableBytes
writeByte bytes addr@(ByteAddress address) value
    | isOutOfRange addr (size bytes) = error "Address is out of range"
    | otherwise                      = ImmutableBytes { originalBytes = originalBytes bytes,
                                                        edits = Map.insert address value (edits bytes)}
                                                        
{- testing
immut1 = new [1,2,3]
immut2 = writeByte immut1 (ByteAddress 0) 4

ASSERT: readByte immut1 (ByteAddress 0) == 1
ASSERT: readByte immut2 (ByteAddress 0) == 4

-}