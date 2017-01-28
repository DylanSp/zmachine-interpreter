module Object
( displayObjectTable
, displayObjectTree
) where 

import Data.Maybe (fromMaybe)
import Numeric (showHex)
import Utilities
import qualified Story as S
import qualified ZString as ZS

newtype ObjectBase = ObjectBase ZWord

newtype PropertyDefaultsTable = PropertyDefaultsTable ZWord

newtype ObjectTreeBase = ObjectTreeBase ZWord

--valid object numbers start at 1
newtype ObjectNumber = ObjectNumber Int deriving (Eq)

--object 0 is the invalid object, the parent of all top-level objects and the empty sibling/child node
invalidObject :: ObjectNumber
invalidObject = ObjectNumber 0

newtype ObjectAddress = ObjectAddress ZWord

newtype PropertyHeaderAddress = PropertyHeaderAddress ZWord

objectTableBase :: S.Story -> ObjectBase
objectTableBase story = ObjectBase (S.readWord story objectTableBaseOffset)
    where objectTableBaseOffset = WordAddress 10
    
defaultPropertyTableSize :: S.Story -> Int
defaultPropertyTableSize story
    | S.isV3OrLower story   = 31
    | otherwise             = 63
    
defaultPropertyTableEntrySize :: Int
defaultPropertyTableEntrySize = 2

treeBase :: S.Story -> ObjectTreeBase
treeBase story = ObjectTreeBase $ fromIntegral (fromIntegral base + defaultPropertyTableEntrySize * tableSize)
    where (ObjectBase base) = objectTableBase story
          tableSize = defaultPropertyTableSize story
          
--version 3 has 4 bytes of attributes, 3 bytes of parent/sibling/child, and 2 bytes of property header address
--version 4 has 6, 6 and 2 respectively.
entrySize :: S.Story -> Int
entrySize story = if S.isV3OrLower story then 9 else 14
    
objectAddress :: S.Story -> ObjectNumber -> ObjectAddress
objectAddress story (ObjectNumber obj) = ObjectAddress $ fromIntegral $ fromIntegral objTreeBase + (obj - 1) * (entrySize story)
    where (ObjectTreeBase objTreeBase) = treeBase story
    
--given the number of an object, find the number of its parent
getParent :: S.Story -> ObjectNumber -> ObjectNumber
getParent story obj
    | S.isV3OrLower story = ObjectNumber $ fromIntegral $ S.readByte story (ByteAddress (4 + fromIntegral addr))
    | otherwise           = ObjectNumber $ fromIntegral $ S.readWord story (WordAddress (6 + fromIntegral addr))
    where (ObjectAddress addr) = objectAddress story obj
    
--given the number of an object, find the number of its sibling
getSibling :: S.Story -> ObjectNumber -> ObjectNumber
getSibling story obj
    | S.isV3OrLower story = ObjectNumber $ fromIntegral $ S.readByte story (ByteAddress (5 + fromIntegral addr))
    | otherwise           = ObjectNumber $ fromIntegral $ S.readWord story (WordAddress (8 + fromIntegral addr))
    where (ObjectAddress addr) = objectAddress story obj
    
--given the number of an object, find the number of its child
getChild :: S.Story -> ObjectNumber -> ObjectNumber
getChild story obj
    | S.isV3OrLower story = ObjectNumber $ fromIntegral $ S.readByte story (ByteAddress (6 + fromIntegral addr))
    | otherwise           = ObjectNumber $ fromIntegral $ S.readWord story (WordAddress (10 + fromIntegral addr))
    where (ObjectAddress addr) = objectAddress story obj
    
--given the number of an object, get the address of its property block
getPropertyHeaderAddress :: S.Story -> ObjectNumber -> PropertyHeaderAddress
getPropertyHeaderAddress story obj = PropertyHeaderAddress $ S.readWord story (WordAddress $ fromIntegral addr + objectPropertyOffset)
    where objectPropertyOffset = if S.isV3OrLower story then 7 else 12
          (ObjectAddress addr) = objectAddress story obj
          
objectName :: S.Story -> ObjectNumber -> Maybe String
objectName story obj
    | len == 0  = Nothing
    | otherwise = Just $ ZS.readZString story (ZS.ZStringAddress $ addr + 1)
    where 
        (PropertyHeaderAddress addr) = getPropertyHeaderAddress story obj
        len = S.readByte story (ByteAddress $ fromIntegral addr)
        
--assumes the convention that properties for object 1 immediately follow the last object entry
countObjects :: S.Story -> Int
countObjects story = fromIntegral (tableEnd - tableStart) `div` size
    where (ObjectTreeBase tableStart) = treeBase story
          (PropertyHeaderAddress tableEnd) = getPropertyHeaderAddress story (ObjectNumber 1)
          size = entrySize story
          
displayObjectTable :: S.Story -> String
displayObjectTable story = concatenateIndexedStrings toString 1 (1 + countObjects story)
    where toString i = showHex i "" ++ ": " 
                         ++ showHex parent "" ++ " "
                         ++ showHex sibling "" ++ " "
                         ++ showHex child "" ++ " "
                         ++ fromMaybe "<unnamed>" name
                         ++ "\n"
            where currentObj = ObjectNumber i
                  (ObjectNumber parent) = getParent story currentObj
                  (ObjectNumber sibling) = getSibling story currentObj
                  (ObjectNumber child) = getChild story currentObj
                  name = objectName story currentObj

getRoots :: S.Story -> [ObjectNumber]
getRoots story = map ObjectNumber rootNums
    where rootNums = filter isRoot [1..countObjects story]
          isRoot obj = getParent story (ObjectNumber obj) == invalidObject
                  
displayObjectTree :: S.Story -> String
displayObjectTree story = concatenateStringReps toString (getRoots story)
    where toString obj = aux "" "" obj
          aux acc indent obj
            | obj == invalidObject  = acc
            | otherwise             = aux withChildren indent sibling
            where name = fromMaybe "<unnamed>" (objectName story obj)
                  child = getChild story obj
                  sibling = getSibling story obj
                  objectText = indent ++ name ++ "\n"
                  withObject = acc ++ objectText
                  newIndent = "  " ++ indent
                  withChildren = aux withObject newIndent child