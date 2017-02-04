--{-# LANGUAGE BinaryLiterals #-}
--put this in Cabal file

module Instruction
( Instruction
, InstructionAddress(..)
, decode
, display
) where 

import Data.Word
import Numeric (showHex)

import Utilities
import qualified Story as S
import OpcodeTypes
import qualified ZString as ZS

newtype InstructionAddress = InstructionAddress Int

data OpcodeForm = LongForm
                | ShortForm
                | VariableForm
                | ExtendedForm
                deriving (Eq)
                
data OperandCount = OP0
                  | OP1
                  | OP2
                  | VAR
                  deriving (Eq)
                  
data OperandType = LargeOperand
                 | SmallOperand
                 | VariableOperand
                 | Omitted
                 deriving (Eq)
                 
--note that my Local/LocalVariable are switched from Lippert's,
--so that LocalVariable/GlobalVariable constructors have the same name as the type
                 
newtype LocalVariable = LocalVariable Int
                
newtype GlobalVariable = GlobalVariable Int
                 
data VariableLocation = Stack
                      | Local LocalVariable
                      | Global GlobalVariable
                      
data Operand = Large Int
             | Small Int
             | Variable VariableLocation
                 
data BranchAddress = ReturnTrue
                   | ReturnFalse
                   | BranchTargetAddress InstructionAddress
                 
instance Show BranchAddress where
    show ReturnTrue = "True"
    show ReturnFalse = "False"
    show (BranchTargetAddress (InstructionAddress addr)) = show addr
                 
data Branch = Branch Bool BranchAddress


data Instruction = Instruction
    { instrOpcode :: Bytecode
    , instrAddress :: InstructionAddress
    , instrLength :: Int
    , instrOperands :: [Operand]
    , instrStore :: Maybe VariableLocation
    , instrBranch :: Maybe Branch
    , instrText :: Maybe String
    }

                 
--useful so we don't have to keep deconstructing InstructionAddress
readByte :: S.Story -> InstructionAddress -> Word8
readByte story (InstructionAddress address) = S.readByte story (ByteAddress address)
                  
{- Spec 4.3:
  Each instruction has a form (long, short, extended or variable)  ...
  If the top two bits of the opcode are $$11 the form is variable;
  if $$10, the form is short. If the opcode is 190 ($BE in hexadecimal)
  and the version is 5 or later, the form is "extended". Otherwise,
  the form is "long". -}
  
--use binary literals in Stack, with newer GHC version. 0b11 instead of 3, 0b10 instead of 2.
decodeForm :: S.Story -> InstructionAddress -> OpcodeForm
decodeForm story address
    | top2bits == 3 = VariableForm
    | top2bits == 2 = if byte == 190 then ExtendedForm else ShortForm
    | otherwise     = LongForm
    where byte = readByte story address
          top2bits = fetchBits bit7 size2 byte
    
{- Spec:
  * Each instruction has ... an operand count (0OP, 1OP, 2OP or VAR).
  * In short form, bits 4 and 5 of the opcode byte ... If this is $11
    then the operand count is 0OP; otherwise, 1OP.
  * In long form the operand count is always 2OP.
  * In variable form, if bit 5 is 0 then the count is 2OP; if it is 1,
    then the count is VAR.
  * In extended form, the operand count is VAR. -}
    
--replace 3 with 0b11
decodeOpCount :: S.Story -> InstructionAddress -> OpcodeForm -> OperandCount
decodeOpCount story address form 
    | form == ShortForm     = if fetchBits bit5 size2 b == 3 then OP0 else OP1
    | form == LongForm      = OP2
    | form == VariableForm  = if fetchBit bit5 b then VAR else OP2
    | form == ExtendedForm  = VAR
    where b = readByte story address
    
    
{-
* Spec :
  * In short form, ... the opcode number is given in the bottom 4 bits.
  * In long form ... the opcode number is given in the bottom 5 bits.
  * In variable form, ... the opcode number is given in the bottom 5 bits.
  * In extended form, ... the opcode number is given in a second opcode byte. *)

    from Lippert, https://ericlippert.com/2016/03/09/canyon-view/
  (* Now what the spec does not say here clearly is: we have just read 4, 5 or
     8 bits, but we need to figure out which of 100+ opcodes we're talking
     about. The location of the bits depends on the form, but the meaning of
     of the bits depends on the operand count. In fact the operation count
     is far more relevant here. It took me some time to puzzle out this
     section of the spec. The spec could more clearly say:
   * In extended form the EXT opcode number is given in the following byte. Otherwise:
   * If the operand count is 0OP then the 0OP opcode number is given in
     the lower 4 bits.
   * If the operand count is 1OP then the 1OP opcode number is given in
     the lower 4 bits.
   * if the operand count is 2OP then the 2OP opcode number is given in
     the lower 5 bits
   * If the operand count is VAR then the VAR opcode number is given in
     the lower 5 bits
  *)
-}  

decodeOpcode :: S.Story -> InstructionAddress -> OpcodeForm -> OperandCount -> Bytecode
decodeOpcode story (InstructionAddress address) ExtendedForm _ = 
    if ext > maxExtended then ILLEGAL else extBytecodes !! fromIntegral ext
        where maxExtended = 29
              ext = S.readByte story (incByteAddr (ByteAddress address))
decodeOpcode story address form opCount
    | opCount == OP0    = zeroOperandBytecodes !! (fetchBits bit3 size4 b)
    | opCount == OP1    = oneOperandBytecodes !! (fetchBits bit3 size4 b)
    | opCount == OP2    = twoOperandBytecodes !! (fetchBits bit4 size5 b)
    | opCount == VAR    = varOperandBytecodes !! (fetchBits bit4 size5 b)
    where b = fromIntegral $ readByte story address
    
getOpcodeLength :: OpcodeForm -> Int
getOpcodeLength ExtendedForm = 2
getOpcodeLength _ = 1

{-
 (* Spec:
  There are four 'types' of operand. These are often specified by a
  number stored in 2 binary digits:
  * $$00 Large constant (0 to 65535) 2 bytes
  * $$01 Small constant (0 to 255) 1 byte
  * $$10 Variable 1 byte
  * $$11 Omitted altogether 0 bytes *)
-}

decodeTypes :: (Num a, Eq a) => a -> OperandType
decodeTypes 0 = LargeOperand
decodeTypes 1 = SmallOperand
decodeTypes 2 = VariableOperand
decodeTypes _ = Omitted
    
{-
(* Spec 4.4
  Next, the types of the operands are specified.
  * In short form, bits 4 and 5 of the opcode give the type.
  * In long form, bit 6 of the opcode gives the type of the first operand,
    bit 5 of the second. A value of 0 means a small constant and 1 means a
    variable.
  * In variable or extended forms, a byte of 4 operand types is given next.
    This contains 4 2-bit fields: bits 6 and 7 are the first field, bits 0 and
    1 the fourth. The values are operand types as above. Once one type has
    been given as 'omitted', all subsequent ones must be.
  * In the special case of the "double variable" VAR opcodes call_vs2 and
    call_vn2 a second byte of types is given, containing the types for the
    next four operands. *)

Lippert:
  (* Once again this could be more clearly written; the spec never calls
     out for instance the obvious fact that 0OP codes have no operand types.
     The logic is:
  * If the count is 0OP then there are no operand types.
  * If the count is 1OP then bits 4 and 5 of the opcode give the type
  * In long form the count is 2OP; bit 6 ... *)
-}

decodeVariableTypes :: Word8 -> [OperandType]
decodeVariableTypes typeByte = aux 0 []
    where aux i acc 
            | i > 3     = acc
            | otherwise = case decodeTypes typeBits of
                      Omitted -> aux (i + 1) acc
                      t -> aux (i + 1) (t : acc)
            where typeBits = fetchBits (BitNumber (i * 2 + 1)) size2 typeByte

--should Omitted be appended by this? could cause problems in decodeOperands
decodeVariableTypes' :: Word8 -> [OperandType]
decodeVariableTypes' typeByte = removeOmitted $ map aux [7, 5, 3, 1]
    where aux i = decodeTypes (fetchBits (BitNumber i) size2 typeByte)
          removeOmitted types = takeWhile (/= Omitted) types ++ [Omitted]

decodeTwoByteOperandTypes :: S.Story -> InstructionAddress -> OpcodeForm -> [OperandType]
decodeTwoByteOperandTypes story (InstructionAddress address) form = decodeVariableTypes typeByte0 ++ decodeVariableTypes typeByte1
    where opcodeLength = getOpcodeLength form
          --S.readByte is used because incByteAddrBy returns a ByteAddress
          typeByte0 = S.readByte story (incByteAddrBy (ByteAddress address) opcodeLength)
          typeByte1 = S.readByte story (incByteAddrBy (ByteAddress address) (opcodeLength + 1))


decodeOperandTypes :: S.Story -> InstructionAddress -> OpcodeForm -> OperandCount -> Bytecode -> [OperandType]
decodeOperandTypes story addr@(InstructionAddress address) form opCount opcode = case (form, opCount, opcode) of
    (_, OP0, _) -> []
    (_, OP1, _) -> [decodeTypes (fetchBits bit5 size2 b)]
        where b = readByte story addr
    (LongForm, _, _) -> case (fetchBits bit6 size2 b) of
        0 -> [SmallOperand, SmallOperand]
        1 -> [SmallOperand, VariableOperand]
        2 -> [VariableOperand, SmallOperand]
        3 -> [VariableOperand, VariableOperand] 
        where b = readByte story addr
    (VariableForm, _, VAR_236) -> decodeTwoByteOperandTypes story addr form
    (VariableForm, _, VAR_250) -> decodeTwoByteOperandTypes story addr form
    (_, _, _)  -> decodeVariableTypes typeByte
        where opcodeLength = getOpcodeLength form
              --S.readByte is used because incByteAddrBy returns a ByteAddress
              typeByte = S.readByte story (incByteAddrBy (ByteAddress address) opcodeLength) 
              
--how many bytes past the opcode were used for type information
getTypeLength :: OpcodeForm -> Bytecode -> Int
getTypeLength VariableForm VAR_236 = 2
getTypeLength VariableForm VAR_250 = 2
getTypeLength VariableForm _ = 1
getTypeLength _  _ = 0
    
--decodes from the bytes in the instruction to a VariableLocation
--polymorphism doesn't work; refactor things using Int to use (Integral a) => ?
decodeVariableOperand :: Int -> VariableLocation
decodeVariableOperand n 
    | n == 0            = Stack
    | n <= maximumLocal = Local (LocalVariable n)
    | otherwise         = Global (GlobalVariable n)
    where maximumLocal = 15
    
--encodes a VariableLocation into a byte/word
--polymorphism doesn't work; refactor things using Int to use (Integral a) => ?
encodeVariableOperand :: VariableLocation -> Int
encodeVariableOperand Stack = 0
encodeVariableOperand (Local (LocalVariable n)) = n
encodeVariableOperand (Global (GlobalVariable n)) = n

{-
decodeOperands :: ByteAddress -> [OperandType] -> [Operand]
decodeOperands address types = map decodeOperand types
    where decodeOperand :: OperandType -> Operand
          decodeOperand 
-}

--refactor into a map? would need a [ByteAddress] though; not sure how to generate that without decoding
--could generate a list of offsets from the [OperandType]
decodeOperands :: S.Story -> ByteAddress -> [OperandType] -> [Operand]
decodeOperands _ _ [] = []
decodeOperands story addr@(ByteAddress address) (LargeOperand : remainingTypes) = w : rest
    where w = Large $ fromIntegral $ S.readWord story (WordAddress address)
          rest = decodeOperands story (incByteAddrBy addr zWordSize) remainingTypes
decodeOperands story addr (SmallOperand : remainingTypes) = b : rest
    where b = Small $ fromIntegral $ S.readByte story addr
          rest = decodeOperands story (incByteAddr addr) remainingTypes
decodeOperands story addr (VariableOperand : remainingTypes) = v : rest
    where b = S.readByte story addr
          v = Variable $ decodeVariableOperand $ fromIntegral b
          rest = decodeOperands story (incByteAddr addr) remainingTypes
decodeOperands story addr (Omitted : remainingTypes) = error "Omitted-type operand passed to decodeOperands"
    
getOperandLength :: [OperandType] -> Int
getOperandLength [] = 0
getOperandLength (LargeOperand : remainingTypes) = zWordSize + getOperandLength remainingTypes
getOperandLength (t : remainingTypes) = 1 + getOperandLength remainingTypes

--opcodes that have a store in all versions  
commonStoreOpcodes :: [Bytecode]
commonStoreOpcodes = [
    OP2_8, OP2_9, OP2_15, OP2_16, OP2_17, OP2_18, OP2_19, 
    OP2_20, OP2_21, OP2_22, OP2_23, OP2_24, OP2_25,
    OP1_129, OP1_130, OP1_131, OP1_132, OP1_136, OP1_142,
    VAR_224, VAR_231, VAR_236, VAR_246, VAR_247, VAR_248,
    EXT_0, EXT_1, EXT_2, EXT_3, EXT_4, EXT_9,
    EXT_10, EXT_19, EXT_29 ]
  
--note that my version uses Story instead of Version, and has story first
--following my convention of using story as the first argument
hasStore :: S.Story -> Bytecode -> Bool
hasStore story opcode 
    | opcode == OP1_143     = S.isV4OrLower story   --"call_1n" in v5, "not" in v1-4
    | opcode == OP0_181     = S.isV4OrHigher story  --"save" branches in v3, stores in v4
    | opcode == OP0_182     = S.isV4OrHigher story  --"restore" branches in v3, stores in v4
    | opcode == OP0_185     = S.isV4OrHigher story  --"pop" in v4, "catch" in v5
    | opcode == VAR_233     = S.getVersion story == S.V6  --TODO: document
    | opcode == VAR_228     = S.isV5OrHigher story  --TODO: document
    | otherwise             = opcode `elem` commonStoreOpcodes
    
{-
 (* Spec 4.6:
  "Store" instructions return a value: e.g., mul multiplies its two
  operands together. Such instructions must be followed by a single byte
  giving the variable number of where to put the result. *)

  Lippert:
  (* This is straightforward but I note something odd; the wording above
    implies that the instruction has ended after the operands, and that
    the store (and hence also branch and text) *follow* the instruction.
    I cannot get behind this. The store, branch and text are all part of
    an instruction. *)
-}

--note that my version uses Story instead of Version, and has story first
--following my convention of using story as the first argument
decodeStore :: S.Story -> ByteAddress -> Bytecode -> Maybe VariableLocation
decodeStore story storeAddress opcode 
    | hasStore story opcode = Just (decodeVariableOperand storeByte)
    | otherwise             = Nothing
    where storeByte = fromIntegral $ S.readByte story storeAddress
    
--note that my version uses Story instead of Version, and has story first
--following my convention of using story as the first argument
getStoreLength :: S.Story -> Bytecode -> Int
getStoreLength story opcode = if hasStore story opcode then 1 else 0
    
commonBranchOpcodes :: [Bytecode]
commonBranchOpcodes = [
    OP2_1, OP2_2, OP2_3, OP2_4, OP2_5, OP2_6, OP2_7, OP2_10,
    OP1_128, OP1_129, OP1_130, OP0_189, OP0_191,
    VAR_247, VAR_255,
    EXT_6, EXT_14, EXT_24, EXT_27 ]
    
--note that my version uses Story instead of Version, and has story first
--following my convention of using story as the first argument
hasBranch :: S.Story -> Bytecode -> Bool
hasBranch story opcode 
    | opcode == OP0_181     = S.isV3OrLower story  --"save" branches in v3, stores in v4
    | opcode == OP0_182     = S.isV3OrLower story  --"restore" branches in v3, stores in v4
    | otherwise             = opcode `elem` commonBranchOpcodes
    
{-
  (* Spec 4.7
  * Instructions which test a condition are called "branch" instructions.
  * The branch information is stored in one or two bytes, indicating what to
    do with the result of the test.
  * If bit 7 of the first byte is 0, a branch occurs when the condition was
    false; if 1, then branch is on true.
  * If bit 6 is set, then the branch occupies 1 byte only, and the "offset"
    is in the range 0 to 63, given in the bottom 6 bits.
  * If bit 6 is clear, then the offset is a signed 14-bit number given in
    bits 0 to 5 of the first byte followed by all 8 of the second.
  * An offset of 0 means "return false from the current routine", and 1 means
    "return true from the current routine".
  * Otherwise, a branch moves execution to the instruction at address
    (Address after branch data) + Offset - 2. *)
-}

decodeBranchOffset :: S.Story -> ByteAddress -> Word8 -> Int
decodeBranchOffset story branchCodeAddress high
    | fetchBit bit6 high    = fromIntegral bottom6
    | unsigned < 8192       = unsigned
    | otherwise             = unsigned - 16384
    where bottom6 = fromIntegral $ fetchBits bit5 size6 high
          low = S.readByte story (incByteAddr branchCodeAddress)
          unsigned = fromIntegral $ 256 * bottom6 + low  --TODO: watch this for potential overflow

decodeBranchTarget :: ByteAddress -> Word8 -> Int -> BranchAddress
decodeBranchTarget branchCodeAddress high offset = BranchTargetAddress branchTarget
    where branchLength = if fetchBit bit6 high then 1 else 2
          (ByteAddress addressAfter) = incByteAddrBy branchCodeAddress branchLength
          branchTarget = InstructionAddress (addressAfter + offset - 2)

decodeBranch :: S.Story -> ByteAddress -> Bytecode -> Maybe Branch
decodeBranch story branchCodeAddress opcode
    | hasBranch story opcode    = Just branch
    | otherwise                 = Nothing
    where high = S.readByte story branchCodeAddress
          sense = fetchBit bit7 high
          offset = decodeBranchOffset story branchCodeAddress high
          branch = case offset of 
                   0 -> Branch sense ReturnFalse
                   1 -> Branch sense ReturnTrue
                   _ -> Branch sense (decodeBranchTarget branchCodeAddress high offset)
          
getBranchLength :: S.Story -> ByteAddress -> Bytecode -> Int
getBranchLength story branchCodeAddress opcode
    | hasBranch story opcode    = if fetchBit bit6 b then 1 else 2
    | otherwise                 = 0
    where b = S.readByte story branchCodeAddress
    
{-
  (* Spec:
    Two opcodes, print and print_ret, are followed by a text string. *)
    TODO: where in Spec?
-}

--text opcodes are the same in all versions
hasText :: Bytecode -> Bool
hasText opcode = opcode == OP0_178 || opcode == OP0_179

decodeText :: S.Story -> ZS.ZStringAddress -> Bytecode -> Maybe String
decodeText story textAddress opcode
    | hasText opcode    = Just $ ZS.readZString story textAddress
    | otherwise         = Nothing

getTextLength :: S.Story -> ZS.ZStringAddress -> Bytecode -> Int
getTextLength story textAddress opcode
    | hasText opcode    = ZS.zStringLength story textAddress
    | otherwise         = 0


decode :: S.Story -> InstructionAddress -> Instruction 
decode story addr@(InstructionAddress address) = Instruction
    { instrOpcode = opcode
    , instrAddress = addr
    , instrLength = len
    , instrOperands = operands
    , instrStore = store
    , instrBranch = branch 
    , instrText = text
    }
    where form = decodeForm story addr
          opCount = decodeOpCount story addr form
          opcode = decodeOpcode story addr form opCount
          opcodeLength = getOpcodeLength form
          operandTypes = decodeOperandTypes story addr form opCount opcode
          typeLength = getTypeLength form opcode
          operandAddress = incByteAddrBy (ByteAddress address) (opcodeLength + typeLength)
          operands = decodeOperands story operandAddress operandTypes
          operandLength = getOperandLength operandTypes
          storeAddress = incByteAddrBy operandAddress operandLength
          store = decodeStore story storeAddress opcode
          storeLength = getStoreLength story opcode
          branchCodeAddress = incByteAddrBy storeAddress storeLength
          branch = decodeBranch story branchCodeAddress opcode
          branchLength = getBranchLength story branchCodeAddress opcode
          (ByteAddress ba) = branchCodeAddress
          textAddress = ZS.ZStringAddress (fromIntegral $ ba + branchLength)
          text = decodeText story textAddress opcode
          textLength = getTextLength story textAddress opcode
          len = opcodeLength + typeLength + operandLength + storeLength + 
                branchLength + textLength
          
--Inform convention:
--stack: numbered 0, "sp"
--locals: numbered 1 through 15, "local0" through "local14"
--globals: numbered 16 through 255, "g0" through "g239" 
displayVariable :: VariableLocation -> String
displayVariable Stack = "sp"
displayVariable (Local (LocalVariable local)) = "local" ++ show (local - 1)
displayVariable (Global (GlobalVariable global)) = "g" ++ show (global - 16)

{-
Lippert:
This is a pretty naive approach; there are several instructions where we would like to have a more nuanced display. 
Calls, for example, have as their first operand a packed address of a routine; it would be nice to display that here as an unpacked address. 
Non-conditional jumps have as their operand a relative address to jump to; it would be nice to display that as an absolute address. And so on.
But for the purposes of this blog, I’m just going to print these out naively.
-}
displayOperands :: Instruction -> String
displayOperands instr = concatenateStringReps toString (instrOperands instr)
    where toString (Large large) = showHex large "" ++ " "  --TODO: pad to 4 digits
          toString (Small small) = showHex small "" ++ " "  --TODO: pad to 2 digits
          toString (Variable var) = displayVariable var ++ " "

displayStore :: Instruction -> String
displayStore instr = case instrStore instr of
    Nothing         -> ""
    Just (variable) -> "->" ++ displayVariable variable
          
displayBranch :: Instruction -> String
displayBranch instr = case instrBranch instr of 
    Nothing -> ""
    Just (Branch sense branchAddr) -> case (sense, branchAddr) of 
        (True, ReturnFalse) -> "?false"
        (False, ReturnFalse) -> "?~false"
        (True, ReturnTrue) -> "?true"
        (False, ReturnTrue) -> "?~true"
        (True, branchAddr) -> "?" ++ show branchAddr    --TODO: pad to 4 digits
        (False, branchAddr) -> "?~" ++ show branchAddr  --TODO: pad to 4 digits
        
displayText :: Instruction -> String
displayText instr = case instrText instr of
    Nothing -> ""
    Just (str) -> str

display :: S.Story -> Instruction -> String
display story instr = showHex startAddr "" ++ ": " 
                      ++ opcodeName story (instrOpcode instr) ++ " "
                      ++ displayOperands instr
                      ++ displayStore instr ++ " "
                      ++ displayBranch instr ++ " "
                      ++ displayText instr
                      ++ "\n"
    where (InstructionAddress startAddr) = instrAddress instr
