{-# Language ApplicativeDo #-}
{-# Language AllowAmbiguousTypes #-}
module Mips where

import Clash.Prelude
import Debug.Trace (trace)
import CreateVecFileTH (vecFromFile)
import Language.Haskell.TH (ExpQ)

type Data = BitVector 8
type Instruction = BitVector 32

type RegFile = Vec 8 Data
type RegAddr = BitVector 3
type Addr = Data

data State
  = Fetch1
  | Fetch2
  | Fetch3
  | Fetch4
  | Decode
  | MemAdr
  | LbRead
  | LbWrite
  | SbWrite
  | RTypeEx
  | RTypeWrite
  | AddiEx
  | AddiWrite
  | BeqEx
  | JEx
  | Halt
  deriving (Generic, Undefined, ShowX, BitPack)

data OpType
  = Lb
  | Sb
  | RType
  | Addi
  | Beq
  | J
  deriving (Generic, Undefined, ShowX, BitPack)

data AluSrcA
  = RegisterA
  | PC
  deriving (Generic, Undefined, ShowX, BitPack)

data AluSrcB
  = RegisterB
  | ConstOne
  | Immediate
  | ImmediateShifted
  deriving (Generic, Undefined, ShowX, BitPack)

data PCSource
  = ALUResult
  | ALUDelayed
  | ImmediateOut
  deriving (Generic, Undefined, ShowX, BitPack)

data MemAccess
  = ReadInstruction
  | ReadData
  | WriteData
  deriving (Generic, Undefined, ShowX, BitPack)

data RegWrite
  = FromRType
  | FromITypeAlu
  | FromITypeMem
  | Disabled
  deriving (Generic, Undefined, ShowX, BitPack)

data Control
  = Control
  { memAccess :: MemAccess
  , instWrite :: Maybe (Unsigned 2)
  , aluSrcA :: AluSrcA
  , aluSrcB :: AluSrcB
  , pcWrite :: Bool
  , pcWriteCond :: Bool
  , regWrite :: RegWrite
  , arithType :: ArithType
  , pcSource :: PCSource
  }
  deriving (Generic, Undefined, ShowX, BitPack)

data AluOp
  = Add
  | Sub
  | And
  | Or
  | Slt
  deriving (Generic, Undefined, ShowX, BitPack)

data ArithType
  = CalcInst
  | CalcOffset
  | Compare
  deriving (Generic, Undefined, ShowX, BitPack)

decodeOpType :: BitVector 6 -> Maybe OpType
decodeOpType 0b100000 = Just Lb
decodeOpType 0b101000 = Just Sb
decodeOpType 0b000000 = Just RType
decodeOpType 0b000100 = Just Beq
decodeOpType 0b000010 = Just J
decodeOpType 0b001000 = Just Addi
decodeOpType _ = Nothing -- parse failure

nextState :: State -> Maybe OpType -> State
nextState Fetch1 _ = Fetch2
nextState Fetch2 _ = Fetch3
nextState Fetch3 _ = Fetch4
nextState Fetch4 _ = Decode
nextState Decode (Just Lb) = MemAdr
nextState Decode (Just Sb) = MemAdr
nextState Decode (Just RType) = RTypeEx
nextState Decode (Just Addi) = AddiEx
nextState Decode (Just Beq) = BeqEx
nextState Decode (Just J) = JEx
nextState Decode Nothing = Halt
nextState MemAdr (Just Lb) = LbRead
nextState MemAdr (Just Sb) = SbWrite
nextState MemAdr (Just _) = Halt
nextState MemAdr Nothing = Halt
nextState LbRead _ = LbWrite
nextState LbWrite _ = Fetch1
nextState SbWrite _ = Fetch1
nextState RTypeEx _ = RTypeWrite
nextState RTypeWrite _ = Fetch1
nextState AddiEx _ = AddiWrite
nextState AddiWrite _ = Fetch1
nextState BeqEx _ = Fetch1
nextState JEx _ = Fetch1
nextState Halt _ = Halt

defaultControl :: Control
defaultControl
  = Control 
  { memAccess = ReadInstruction
  , instWrite = Nothing
  , aluSrcA = PC
  , aluSrcB = RegisterB
  , pcWrite = False
  , pcWriteCond = False
  , regWrite = Disabled
  , arithType = CalcOffset
  , pcSource = ALUResult
  }

controllerT :: State -> Control
controllerT Fetch1 =
  defaultControl { instWrite = Just 3, aluSrcB = ConstOne, pcWrite = True }
controllerT Fetch2 =
  defaultControl { instWrite = Just 2, aluSrcB = ConstOne, pcWrite = True }
controllerT Fetch3 =
  defaultControl { instWrite = Just 1, aluSrcB = ConstOne, pcWrite = True }
controllerT Fetch4 =
  defaultControl { instWrite = Just 0, aluSrcB = ConstOne, pcWrite = True }
controllerT Decode =
  defaultControl { aluSrcB = ImmediateShifted }
controllerT MemAdr =
  defaultControl { aluSrcA = RegisterA, aluSrcB = Immediate }
controllerT LbRead =
  defaultControl { memAccess = ReadData }
controllerT LbWrite =
  defaultControl { regWrite = FromITypeMem }
controllerT SbWrite =
  defaultControl { memAccess = WriteData }
controllerT RTypeEx =
  defaultControl { aluSrcA = RegisterA, arithType = CalcInst }
controllerT RTypeWrite =
  defaultControl { regWrite = FromRType }
controllerT AddiEx =
  defaultControl { aluSrcA = RegisterA, aluSrcB = Immediate, arithType = CalcOffset }
controllerT AddiWrite =
  defaultControl { regWrite = FromITypeAlu }
controllerT BeqEx =
  defaultControl { aluSrcA = RegisterA, arithType = Compare, pcSource = ALUDelayed, pcWriteCond = True }
controllerT JEx =
  defaultControl { pcWrite = True, pcSource = ImmediateOut }
controllerT Halt = defaultControl

controller :: HiddenClockResetEnable dom => Signal dom (Maybe OpType) -> Signal dom Control
controller = moore nextState controllerT Fetch1
--controller :: HiddenClockResetEnable dom => Signal dom (Maybe OpType) -> Signal dom (State, Control)
--controller = moore nextState (\st -> (st, controllerT st)) Fetch1

updateInst :: Maybe (Unsigned 2) -> Data -> Instruction -> Maybe Instruction
updateInst maybeIndex partialInst oldInst =
    update' <$> maybeIndex
  where
    update' :: Unsigned 2 -> Instruction
    update' 0 = setSlice d7  d0 partialInst oldInst
    update' 1 = setSlice d15 d8 partialInst oldInst
    update' 2 = setSlice d23 d16 partialInst oldInst
    update' 3 = setSlice d31 d24 partialInst oldInst

instReg :: HiddenClockResetEnable dom
        => Signal dom (Maybe (Unsigned 2))
        -> Signal dom Data
        -> Signal dom Instruction
instReg instWriteSelect instData = reg
  where
    reg = regMaybe 0 $ liftA3 updateInst instWriteSelect instData reg

aluControl :: ArithType -> BitVector 6 -> AluOp
aluControl CalcInst 0b100000 = Add
aluControl CalcInst 0b100010 = Sub
aluControl CalcInst 0b100100 = And
aluControl CalcInst 0b100101 = Or
aluControl CalcInst 0b101010 = Slt
--aluControl CalcInst x = trace (("unknown OpType: " :: String) <> (show x)) undefined
aluControl CalcInst _ = Add
aluControl CalcOffset _ = Add
aluControl Compare _ = Sub

alu :: AluOp -> Data -> Data -> Data
alu Add a b = a + b
alu Sub a b = a - b
alu And a b = a .&. b
alu Or a b = a .|. b
alu Slt a b = if a < b then 1 else 0

regFileT :: RegFile -> (RegAddr, RegAddr, Maybe (RegAddr, Data)) -> (RegFile, (Data, Data))
regFileT mem (regAddrA, regAddrB, maybeWrite) = (mem', (readReg regAddrA, readReg regAddrB))
  where
    readReg i = if i == 0 then 0 else mem !! i
    mem' =
      case maybeWrite of
        Just (writeRegAddr, writeData) -> replace writeRegAddr writeData mem
        Nothing -> mem

regFile :: HiddenClockResetEnable dom
        => Signal dom RegAddr
        -> Signal dom RegAddr
        -> Signal dom (Maybe (RegAddr, Data))
        -> Signal dom (Data, Data)
regFile regAddrA regAddrB maybeWrite =
  mealy regFileT (replicate d8 0) $ bundle (regAddrA, regAddrB, maybeWrite)

liftA4 :: Applicative f
        => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f fa fb fc fd = pure f <*> fa <*> fb <*> fc <*> fd

pcNext :: PCSource -> Addr -> Addr -> Addr -> Addr
pcNext ALUResult aluResult _ _ = aluResult
pcNext ALUDelayed _ aluOut _ = aluOut
pcNext ImmediateOut _ _ immShifted = immShifted

muxSrcA :: AluSrcA -> Data -> Data -> Data
muxSrcA RegisterA regA _ = regA
muxSrcA PC _ pc = pc

muxSrcB :: AluSrcB -> Data -> Data -> Data -> Data
muxSrcB RegisterB regB _ _ = regB
muxSrcB ConstOne _ _ _ = 1
muxSrcB Immediate _ imm _ = imm
muxSrcB ImmediateShifted _ _ immShifted = immShifted

maybeRegWrite :: RegWrite -> Instruction -> Data -> Data -> Maybe (RegAddr, Data)
maybeRegWrite Disabled _ _ _ = Nothing
maybeRegWrite FromRType inst _ aluOut = Just (slice d13 d11 inst, aluOut)
maybeRegWrite FromITypeAlu inst _ aluOut = Just (slice d18 d16 inst, aluOut)
maybeRegWrite FromITypeMem inst memData _ = Just (slice d18 d16 inst, memData)

memReadWrite :: MemAccess -> Addr -> Addr -> Data -> (Addr, Maybe Data)
memReadWrite ReadInstruction pc _ _ = (pc, Nothing)
memReadWrite ReadData _ aluOut _ = (aluOut, Nothing)
memReadWrite WriteData _ aluOut regB = (aluOut, Just regB)

type CpuOutput = (Bit, Addr, Data)

hardwareTranslate :: (Addr, Maybe Data) -> CpuOutput
hardwareTranslate (addr, Nothing) = (0, addr, 0)
hardwareTranslate (addr, Just dat) = (1, addr, dat)

top :: HiddenClockResetEnable dom => Signal dom Data -> Signal dom CpuOutput
top memDataSig = hardwareTranslate <$> memAccessSig
--top memDataSig = bundle (pcSig, stateSig, maybeRegWriteSig, memAccessSig)
  where
    zeroSig = (== 0) <$> aluResultSig
    pcEnSig =
      (pcWrite <$> controlSig) .||.
        ((pcWriteCond <$> controlSig) .&&. zeroSig)
    pcNextSig =
      liftA4 pcNext (pcSource <$> controlSig) aluResultSig aluOutSig immShiftedSig
    pcSig = regEn 0 pcEnSig pcNextSig
    immSig = slice d7 d0 <$> instSig
    immShiftedSig = (* 4) <$> immSig
    instSig = instReg (instWrite <$> controlSig) memDataSig
    opTypeSig = decodeOpType . slice d31 d26 <$> instSig
    controlSig = controller opTypeSig
    --(stateSig, controlSig) = unbundle $ controller opTypeSig
    regAAddr = slice d23 d21 <$> instSig
    regBAddr = slice d18 d16 <$> instSig
    (regASig, regBSig) =
      unbundle $ register (0, 0) $ regFile regAAddr regBAddr maybeRegWriteSig
    aluControlSig =
      liftA2 aluControl (arithType <$> controlSig) (slice d5 d0 <$> instSig)
    srcASig = liftA3 muxSrcA (aluSrcA <$> controlSig) regASig pcSig
    srcBSig = liftA4 muxSrcB (aluSrcB <$> controlSig) regBSig immSig immShiftedSig
    aluResultSig = liftA3 alu aluControlSig srcASig srcBSig
    aluOutSig = register 0 aluResultSig
    maybeRegWriteSig =
      liftA4 maybeRegWrite (regWrite <$> controlSig) instSig (register 0 memDataSig) aluOutSig
    memAccessSig =
      liftA4 memReadWrite (memAccess <$> controlSig) pcSig aluOutSig regBSig

$(createDomain (knownVDomain @System){vTag="SystemSync", vReset=Synchronous})

topEntity
  :: Clock SystemSync
  -> Reset SystemSync
  -> Enable SystemSync
  -> Signal SystemSync Data
  -> Signal SystemSync CpuOutput
topEntity = exposeClockResetEnable top
{-# NOINLINE topEntity #-}

--mem :: ExpQ
--mem = [| vecFromFile "./memfile.bin" |] 

ram :: HiddenClockResetEnable dom
  => Signal dom (Addr, Maybe Data)
  -> Signal dom Data
----ram = mealy ramT $(vecFromFile d64 d32 "./fibtest.bin") -- requires addi
--ram = mealy ramT $(vecFromFile d64 d32 "./memfile.bin") -- requires addi
--ram = mealy ramT $(lift $ (vecFromFile "./memfile.bin" :: Vec 64 (BitVector 32))) -- requires addi
ram = mealy ramT $ vecFromFile "./memfile.bin" -- requires addi
  where
    sel :: Vec 64 (BitVector 32) -> Addr -> Data
    sel mem addr =
      let value = mem !! slice d7 d2 addr in
      case slice d1 d0 addr of
        0b00 -> slice d31 d24 value
        0b01 -> slice d23 d16 value
        0b10 -> slice d15 d8 value
        0b11 -> slice d7 d0 value
    update :: Addr -> Data -> Vec 64 (BitVector 32) -> Vec 64 (BitVector 32)
    update addr dat mem =
      let addr' = slice d7 d2 addr  in
      let value = mem !! addr' in
      let value' = case slice d1 d0 addr of
                      0b00 -> setSlice d31 d24 dat value
                      0b01 -> setSlice d23 d16 dat value
                      0b10 -> setSlice d15 d8 dat value
                      0b11 -> setSlice d7 d0 dat value
      in replace addr' value' mem
    ramT mem (addr, Nothing) = (mem, sel mem addr)
    ramT mem (addr, Just dat) = (update addr dat mem, sel mem addr)

{-# ANN ramEntity (Synthesize { t_name = "ram", t_inputs = [PortName "input"], t_output = PortName "output"}) #-}
ramEntity :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Addr, Maybe Data)
  -> Signal System Data
ramEntity = exposeClockResetEnable ram
{-# NOINLINE ramEntity #-}
--
--data TopState
--  = TopState
--  { pc :: Addr
--  , state :: State
--  , regWrite' :: Maybe (RegAddr, Data)
--  , memData :: Data
--  , memAccess' :: (Addr, Maybe Data)
--  } deriving (Generic, Undefined, ShowX, BitPack)
--
--testBench :: Signal System TopState
--testBench = pure TopState <*> pcSig <*> stateSig <*> maybeRegWriteSig <*> memDataSig <*> memAccessSig
--  where
--    memDataSig = ramEntity clk rst en memAccessSig
--    (pcSig, stateSig, maybeRegWriteSig, memAccessSig) = unbundle $ topEntity clk rst en memDataSig
--    en = enableGen
--    clk = tbSystemClockGen (pure True)
--    rst = systemResetGen
