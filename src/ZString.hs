{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ZString
( ZStringAddress (..)
, AbbreviationNumber (..)
, readZString
, displayBytes
, abbreviationZString
, zStringLength
) where

import qualified Data.Char as Char
import qualified Story as S
import Utilities
    
data StringState = Alphabet Int
                 | Abbrev AbbreviationNumber
                 | Leading
                 | Trailing Int

abbrev0 :: StringState
abbrev0 = Abbrev (AbbreviationNumber 0)

abbrev32:: StringState
abbrev32 = Abbrev (AbbreviationNumber 32)

abbrev64 :: StringState
abbrev64 = Abbrev (AbbreviationNumber 64)

alphabet0 :: StringState
alphabet0 = Alphabet 0

alphabet1 :: StringState
alphabet1 = Alphabet 1

alphabet2 :: StringState
alphabet2 = Alphabet 2
    
newtype AbbreviationNumber = AbbreviationNumber Int

newtype AbbrevTableBase = AbbrevTableBase ZWord

newtype WordZStringAddress = WordZStringAddress ZWord

newtype ZStringAddress = ZStringAddress ZWord deriving (Eq, Ord, Num, Enum, Real, Integral)

newtype ZChar = ZChar Int deriving (Eq, Ord, Num, Enum, Real, Integral, Show)

alphabetTables :: [String]
alphabetTables = [" ?????abcdefghijklmnopqrstuvwxyz",
                  " ?????ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                  " ??????\n0123456789.,!?_#\'\"/\\-:()"]

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
    
readZString :: S.Story -> ZStringAddress -> String
readZString story (ZStringAddress address) = aux "" alphabet0 (WordAddress (fromIntegral address))
    where processZChar :: ZChar -> StringState -> (String, StringState)
          processZChar (ZChar 1) (Alphabet _) = ("", abbrev0)
          processZChar (ZChar 2) (Alphabet _) = ("", abbrev32)
          processZChar (ZChar 3) (Alphabet _) = ("", abbrev64)
          processZChar (ZChar 4) (Alphabet _) = ("", alphabet1)
          processZChar (ZChar 5) (Alphabet _) = ("", alphabet2)
          processZChar (ZChar 6) (Alphabet 2) = ("", Leading)
          processZChar (ZChar char) (Alphabet a) = ([alphabetTables !! a !! char], alphabet0)
          processZChar (ZChar char) (Abbrev (AbbreviationNumber a)) = (str, alphabet0)
            where abbrv = AbbreviationNumber (a + char)
                  addr = abbreviationZString story abbrv
                  str = readZString story addr
          processZChar (ZChar char) Leading = ("", Trailing char)
          processZChar (ZChar char) (Trailing high) = (s, alphabet0)
            where s = show $ Char.chr (high * 32 + char)
          aux :: String -> StringState -> WordAddress -> String
          aux acc state1 currentAddr
            | isEnd     = newAcc
            | otherwise = aux newAcc nextState (incWordAddr currentAddr)
            where word = S.readWord story currentAddr
                  isEnd = fetchBit bit15 word
                  zCharBitSize = size5
                  zChar1 = ZChar $ fromIntegral $ fetchBits bit14 zCharBitSize word
                  zChar2 = ZChar $ fromIntegral $ fetchBits bit9 zCharBitSize word
                  zChar3 = ZChar $ fromIntegral $ fetchBits bit4 zCharBitSize word
                  (text1, state2) = processZChar zChar1 state1
                  (text2, state3) = processZChar zChar2 state2
                  (text3, nextState) = processZChar zChar3 state3
                  newAcc = acc ++ text1 ++ text2 ++ text3
                  
--Lippert: gives the length in bytes of the encoded zstring, not the decoded string
--TODO - like readZString, this could be refactored into a fold if we could get a [ZStringAddress]
zStringLength :: S.Story -> ZStringAddress -> Int
zStringLength story (ZStringAddress address) = aux 0 (WordAddress (fromIntegral address))
    where aux :: Int -> WordAddress -> Int
          aux len currentAddr
            | isEnd     = len + 2 --string ends
            | otherwise = aux (len + 2) (incWordAddr currentAddr)
            where isEnd = fetchBit bit15 (S.readWord story currentAddr) 
                  
--for debugging
displayBytes :: S.Story -> ZStringAddress -> String
displayBytes story (ZStringAddress address) = aux "" (WordAddress (fromIntegral address))
    where aux :: String -> WordAddress -> String
          aux acc currentAddr 
            | isEnd     = newAcc
            | otherwise = aux newAcc (incWordAddr currentAddr)
            where word = S.readWord story currentAddr
                  isEnd = fetchBit bit15 word
                  --isEnd = True
                  zChar1 = ZChar (fromIntegral $ fetchBits bit14 size5 word)
                  zChar2 = ZChar (fromIntegral $ fetchBits bit9 size5 word)
                  zChar3 = ZChar (fromIntegral $ fetchBits bit4 size5 word)
                  newAcc = acc ++ " " ++ show zChar1 ++ " " ++ show zChar2 ++ " " ++ show zChar3
