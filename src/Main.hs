import Data.Word
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import qualified Story as S
import Utilities
import ZString as ZS 
import Dictionary as D
import Object as O

--TODO: make each of these a separate method, have one main that calls all of them

--test: version of minizork.z3 should be 3
{-
version :: S.Story -> Word8
version story = S.readByte story (ByteAddress 0)

main = do
    story <- S.loadStory "minizork.z3"
    putStrLn $ show $ version story
-}


--test: should output description of Flood Control Dam #3
--note: for some reason, doesn't output final quotation mark
{-
main = do
    story <- S.loadStory "minizork.z3"
    let zStringAddr = ZS.ZStringAddress 0xb106
    let text = ZS.readZString story zStringAddr
    putStrLn text
-}

--byte counter; should be 52216 to match wc -c
{-
main = do
    contents <- BS.readFile "minizork.z3"
    let bytes = BS.unpack contents
    putStrLn $ show $ length bytes
-}

--debugging; get total length of story's internal byte lists
--checks out:
--Dynamic length: 135
--Static length: 52081
--Total length: 52216

{-
main = do
    story <- S.loadStory "minizork.z3"
    S.debugStory story
-}

--test: should output story's dictionary
{-
main = do
    story <- S.loadStory "minizork.z3"
    let dict = D.displayDict story
    putStrLn $ dict
-}

--test: should output story's object table
{-
main = do
    story <- S.loadStory "minizork.z3"
    let table = O.displayObjectTable story
    putStrLn $ table
-}

--test: should output story's object tree

main = do
    story <- S.loadStory "minizork.z3"
    let tree = O.displayObjectTree story
    putStrLn $ tree
