module Story 
( Story(..)
, loadStory
, readByte
, writeByte
, readWord
, writeWord
, debugStory
) where

import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Utilities
import qualified ImmutableBytes as IMB

data Story = Story { dynamicMemory :: IMB.ImmutableBytes
                   , staticMemory :: [Word8]
                   }

readByte :: Story -> ByteAddress -> Word8
readByte story address
    | isInRange address dynamicSize = IMB.readByte (dynamicMemory story) address
    | otherwise                     = dereferenceString (staticMemory story) staticAddress 
    where dynamicSize = IMB.size (dynamicMemory story)
          staticAddress = decByteAddrBy address dynamicSize

--separate high', low' are necessary, otherwise 256 * high is done in Word8 arithmetic, where it overflows
readWord :: Story -> WordAddress -> ZWord
readWord story address = fromIntegral $ 256 * high' + low'
    where high = readByte story (addressOfHighByte address)
          high' :: Word16
          high' = fromIntegral high
          low  = readByte story (addressOfLowByte address)
          low' :: Word16
          low' = fromIntegral low
          
writeByte :: Story -> ByteAddress -> Word8 -> Story
writeByte story address value = Story { dynamicMemory = dynamicMemory',
                                        staticMemory = staticMemory'}
    where dynamicMemory' = IMB.writeByte (dynamicMemory story) address value
          staticMemory'  = staticMemory story

writeWord :: Story -> WordAddress -> ZWord -> Story
writeWord story address value = story''
    where high = fromIntegral $ (value `shiftR` 8) .&. 0xFF
          low  = fromIntegral $ value .&. 0xFF
          story'  = writeByte story (addressOfHighByte address) high
          story'' = writeByte story' (addressOfLowByte address) low
          
fileToWordList :: String -> IO [Word8]
fileToWordList filename = do
    contents <- BS.readFile filename
    return $ BS.unpack contents
    
headerSize :: Int
headerSize = 64

staticMemoryBaseOffset :: WordAddress
staticMemoryBaseOffset = WordAddress 14

createStory :: String -> [Word8] -> Story
createStory name raw 
    | len < headerSize || len <= dynamicLength  = error (name ++ " was not a valid story file")
    | otherwise = Story { dynamicMemory = dynamicMemory', staticMemory = static }
    where len = length raw
          high = dereferenceString raw (addressOfHighByte staticMemoryBaseOffset)
          low = dereferenceString raw (addressOfLowByte staticMemoryBaseOffset)
          dynamicLength = fromIntegral $ high * 256 + low
          (dynamic, static) = splitAt dynamicLength raw
          dynamicMemory' = IMB.new dynamic

loadStory :: String -> IO Story
loadStory filename = do
    file <- fileToWordList filename
    return $ createStory filename file
    
debugStory :: Story -> IO ()
debugStory story = do
    let dynamicLen = IMB.size $ dynamicMemory story
    let staticLen = length $ staticMemory story
    let totalLen = dynamicLen + staticLen
    putStrLn $ "Dynamic length: " ++ show dynamicLen
    putStrLn $ "Static length: " ++ show staticLen
    putStrLn $ "Total length: " ++ show totalLen