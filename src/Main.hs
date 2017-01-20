import Data.Word
import qualified Story as S
import Utilities

version :: S.Story -> Word8
version story = S.readByte story (ByteAddress 0)

main = do
    story <- S.loadStory "minizork.z3"
    putStrLn $ show $ version story
    
    
--test: version of minizork.z3 should be 3