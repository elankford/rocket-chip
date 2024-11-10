import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.Parameters

class TracerIO(implicit p: Parameters) extends Bundle {
  val tracerQueueVAddrIn = Input(UInt(64.W))     // Virtual address from tracer queue
  val tracerQueueDataReady = Input(Bool())       // Data ready signal from tracer queue

  val toBeMarked = Output(Bool())                // If address needs marking
  val tracerReadyForNext = Output(Bool())        // Ready for next address
  val toBeMarkedData = Output(UInt(64.W))        // Address to be marked (virtual address)

  val mem = new HellaCacheIO                   // TileLink memory interface
}

class Tracer(implicit p: Parameters) extends Module {
  val io = IO(new TracerIO)

  // FSM State Definitions
  val idle :: translateAddress :: requestMemRead :: waitMemRead :: checkMarkBit :: Nil = Enum(5)
  val state = RegInit(idle)

  // Internal Registers
  val currentVAddr = Reg(UInt(64.W))             // Current virtual address
  val currentPAddr = Reg(UInt(64.W))             // Current physical address
  val memDataReg = Reg(UInt(64.W))               // Data read from memory

  // Outputs Defaults
  io.toBeMarked := false.B
  io.tracerReadyForNext := false.B
  io.toBeMarkedData := 0.U

  // TileLink Memory Access
  io.mem.req.valid := false.B                    // Default: no request
  io.mem.req.bits.addr := currentPAddr           // Physical address for memory read
  io.mem.req.bits.cmd := M_XRD                   // Read command
  io.mem.req.bits.size := log2Ceil(8).U          // 64-bit read
  io.mem.req.bits.signed := false.B

  // TLB for Virtual to Physical Address Translation
  val tlb = Module(new TLB(instruction = false, lgMaxSize = log2Ceil(8), nEntries = 32))
  tlb.io.req.valid := false.B
  tlb.io.req.bits.vaddr := currentVAddr          // Input virtual address
  tlb.io.req.bits.size := log2Ceil(8).U          // Size of address
  tlb.io.req.bits.cmd := M_XRD                   // TLB request command

  switch(state) {
    is(idle) {
      io.tracerReadyForNext := true.B            // Signal ready for next address
      io.toBeMarked := false.B                    // Default: no address to be marked
      when(io.tracerQueueDataReady) {
        currentVAddr := io.tracerQueueVAddrIn    // Capture incoming virtual address
        state := translateAddress
        io.tracerReadyForNext := false.B
      }
    }

    is(translateAddress) {
      tlb.io.req.valid := true.B                 // Initiate address translation
      when(tlb.io.resp.valid) {                  // Wait for valid TLB response
        currentPAddr := tlb.io.resp.bits.paddr   // Capture translated physical address
        state := requestMemRead
      }
    }

    is(requestMemRead) {
      // Initiate memory read using TileLink
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := currentPAddr       // Physical address from translation
      when(io.mem.req.ready) {                   // Proceed if memory is ready
        state := waitMemRead
      }
    }

    is(waitMemRead) {
      // Wait for memory read data to return
      when(io.mem.resp.valid) {
        memDataReg := io.mem.resp.bits.data      // Store data read from memory
        state := checkMarkBit
      }
    }

    is(checkMarkBit) {
        io.toBeMarked := ~memDataReg(0)      // Check if mark bit is set
        io.toBeMarkedData := currentVAddr    
        state := idle
    }
  }
}
