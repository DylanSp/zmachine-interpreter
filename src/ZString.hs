module ZString
(
) where

import qualified Story as S
import Utilities
    
newtype AbbreviationNumber = AbbreviationNumber Int

newtype AbbrevTableBase = AbbrevTableBase ZWord

newtype WordZStringAddress = WordZStringAddress ZWord

newtype ZStringAddress = ZStringAddress ZWord

abbrevTableBaseOffset :: WordAddress
abbrevTableBaseOffset = WordAddress 24

abbrevTableLength :: Int
abbrevTableLength = 96

abbreviationsTableBase :: S.Story -> AbbrevTableBase
abbreviationsTableBase story = AbbrevTableBase (S.readWord story abbrevTableBaseOffset)

decodeWordAddress :: WordZStringAddress -> ZStringAddress
decodeWordAddress (WordZStringAddress address) = ZStringAddress ((fromIntegral address) * 2)

--fromIntegral is necessary to convert ZWord (Word16) to a WordAddress
firstAbbrevAddr :: AbbrevTableBase -> WordAddress
firstAbbrevAddr (AbbrevTableBase base) = WordAddress $ fromIntegral base

abbreviationZString :: S.Story -> AbbreviationNumber -> ZStringAddress
abbreviationZString story (AbbreviationNumber n)
    | n < 0 || n >= abbrevTableLength  = error "Bad offset into abbreviations table"
    | otherwise                        = decodeWordAddress wordAddr
        where base = firstAbbrevAddr (abbreviationsTableBase story)
              abbrevationAddr = incWordAddrBy base n
              wordAddr = WordZStringAddress (S.readWord story abbrevationAddr)
