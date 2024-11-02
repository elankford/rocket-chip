class SweepUnit(params: SweepUnitParams) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val mem = new MemoryInterface // REDEFINE THIS
  })

  // sweep logic
}

class SweepRoCC(opcodes: OpcodeSet) extends RoCC(opcodes) {
  val sweepUnit = Module(new SweepUnit(params))

  // Connect RoCC instructions to control the sweep unit
  when(io.cmd.valid) {
    sweepUnit.io.start := true.B
  }

  io.resp.valid := sweepUnit.io.done
}


class MemoryInterface extends Bundle {
  val addr = Output(UInt(64.W))
  val dataIn = Input(UInt(64.W))
  val dataOut = Output(UInt(64.W))
  val read = Output(Bool())
  val write = Output(Bool())
  val ready = Input(Bool())
}


val sIdle :: sRead :: sProcess :: sWrite :: sDone :: Nil = Enum(5)
val state = RegInit(sIdle)

switch(state) {
  is(sIdle) {
    when(io.start) {
      state := sRead
    }
  }
  is(sRead) {
    // Initiate memory read
    state := sProcess
  }
  is(sProcess) {
    // Check if the object is marked
    // Decide whether to free or skip
    state := sWrite
  }
  is(sWrite) {
    // Write back if necessary
    state := sRead // Or sDone if at the end
  }
  is(sDone) {
    io.done := true.B
    state := sIdle
  }
}


val markBitmap = SyncReadMem(markBitmapSize, Bool())

// Read mark bit
val markBit = markBitmap.read(currentAddr)

// Write mark bit (if needed)
when(writeMarkBit) {
  markBitmap.write(currentAddr, newMarkValue)
}

val freeList = Module(new FreeListManager())

when(!markBit) {
  freeList.io.addFreeBlock(currentAddr)
}

val currentAddr = RegInit(startAddr)

when(state === sRead && io.mem.ready) {
  io.mem.addr := currentAddr
  io.mem.read := true.B
}

when(state === sProcess) {
  // Process the data
  currentAddr := currentAddr + objectSize
}

