module Dictionary
( displayDict
) where

import Data.Word    
import qualified Story as S
import qualified ZString as ZS
import Utilities
    
--base address of the whole dictionary
newtype DictionaryBase = DictionaryBase ZWord

--base address of the dictionary entries
newtype DictionaryTableBase = DictionaryTableBase Int

--address of dictionary entries
newtype DictionaryAddress = DictionaryAddress ZWord

--index into the dictionary table
newtype DictionaryNumber = DictionaryNumber Int

getDictionaryBase :: S.Story -> DictionaryBase
getDictionaryBase story = DictionaryBase (S.readWord story dictionaryBaseOffset)
    where dictionaryBaseOffset = WordAddress 8
    
wordSeparatorsBase :: DictionaryBase -> ByteAddress
wordSeparatorsBase (DictionaryBase base) = ByteAddress (fromIntegral base)

wordSeparatorsCount :: S.Story -> Word8
wordSeparatorsCount story = S.readByte story wsBase
    where dictBase = getDictionaryBase story
          wsBase = wordSeparatorsBase dictBase
          
--fromIntegral necessary to convert Word8 to Int
entryBase :: S.Story -> ByteAddress
entryBase story = incByteAddrBy wsBase (fromIntegral (wsCount + 1))
    where dictBase = getDictionaryBase story
          wsCount = wordSeparatorsCount story
          wsBase = wordSeparatorsBase dictBase
          
--length of each entry
entryLength :: S.Story -> Word8
entryLength story = S.readByte story (entryBase story)

--number of entries in the dictionary
entryCount :: S.Story -> ZWord
entryCount story = S.readWord story (WordAddress address)
    where (ByteAddress address) = incByteAddr $ entryBase story
    
--base address of the dictionary's table of entries
tableBase :: S.Story -> DictionaryTableBase
tableBase story = DictionaryTableBase address
    where (ByteAddress address) = incByteAddrBy (entryBase story) 3
    
entryAddress :: S.Story -> DictionaryNumber -> DictionaryAddress
entryAddress story (DictionaryNumber num) = DictionaryAddress (fromIntegral $ base + num * len)
    where (DictionaryTableBase base) = tableBase story
          len = fromIntegral $ entryLength story
          
entry :: S.Story -> DictionaryNumber -> String
entry story num = ZS.readZString story (ZS.ZStringAddress address)
    where (DictionaryAddress address) = entryAddress story num
    
displayDict :: S.Story -> String
displayDict story = concatenateIndexedStrings toString 0 count
    where count = fromIntegral $ entryCount story
          toString i = entry story (DictionaryNumber i) ++ " "