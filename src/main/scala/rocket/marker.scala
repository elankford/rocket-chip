// import chisel3._
// import chisel3.util._
// import freechips.rocketchip.tilelink._
// import freechips.rocketchip.rocket._
// import freechips.rocketchip.config.Parameters
// import freechips.rocketchip.util._

// // Marker module in Chisel
// class Marker(implicit p: Parameters) extends Module {
//   val io = IO(new Bundle {
//     val markQueueVAddrIn = Input(UInt(64.W))  // 64-bit virtual address from mark queue
//     val markQueueValidIn = Input(Bool())      // Valid signal from mark queue
//     val outboundRefAddrOut = Output(UInt(64.W)) // Outbound reference physical address
//     val outputDoneProcessing = Output(Bool())   
//     val outputRefReady = Output(Bool())    // Signal that a reference address is ready
//     val tlb = new TLBIO()                     // TLB IO for address translation
//     val mem = new HellaCacheIO                 // TileLink memory interface
//   })

//   // Define state machine states
//   val idle :: translateAddr :: loadStatusWord :: setMarkBit :: loadNumRefs :: processRefs :: outputRef :: waitRefOffset :: loadRefOffset :: Nil = Enum(9)
//   val state = RegInit(idle)

//   // Internal registers
//   val objVAddr = RegInit(0.U(64.W))          // Object virtual address
//   val objPAddr = RegInit(0.U(64.W))          // Object physical address
//   val statusWord = RegInit(0.U(64.W))        // Header status word from memory
//   val tibVAddr = RegInit(0.U(64.W))          // TIB virtual address
//   val tibPAddr = RegInit(0.U(64.W))          // TIB physical address
//   val numRefs = RegInit(0.U(64.W))           // Number of outbound references
//   val refOffsetsAddr = RegInit(0.U(64.W))    // Address of the reference offsets
//   val refCounter = RegInit(0.U(32.W))        // Reference counter
//   val objBasePAddr = RegInit(0.U(64.W))      // Base physical address of the object
//   val refOffset = RegInit(0.U(64.W))         // Offset of the reference
//   val refAddr = RegInit(0.U(64.W))           // Physical address of the reference

//   // Output control
//   io.outboundRefAddrOut := refAddr
//   io.outputRefReady := false.B

//   // Memory and TLB control signals
//   io.mem.req.valid := false.B
//   io.mem.req.bits.cmd := M_XRD // Default read command, will be adjusted as necessary
//   io.mem.req.bits.size := log2Ceil(8).U // 64-bit accesses
//   io.mem.req.bits.addr := objPAddr

//   // State machine
//   switch(state) {
//     is(idle) {
//       io.outputDoneProcessing := true.B
//       io.outputRefReady := false.B
//       when(io.markQueueValidIn) {
//         objVAddr := io.markQueueVAddrIn
//         state := translateAddr
//         io.outputDoneProcessing := false.B
//       }
//     }

//     is(translateAddr) {
//       // Request address translation via TLB
//       io.tlb.req.valid := true.B
//       io.tlb.req.bits.vaddr := objVAddr
//       io.tlb.req.bits.size := log2Ceil(8).U
//       when(io.tlb.resp.valid) {
//         objPAddr := io.tlb.resp.bits.paddr
//         state := loadStatusWord
//       }
//     }

//     is(loadStatusWord) {
//       // Load status word from object physical address
//       io.mem.req.valid := true.B
//       io.mem.req.bits.addr := objPAddr
//       io.mem.req.bits.cmd := M_XRD
//       when(io.mem.resp.valid) {
//         statusWord := io.mem.resp.bits.data
//         state := setMarkBit
//       }
//     }

//     is(setMarkBit) {
//       // Set the mark bit (LSB of status word) and write back
//       statusWord := statusWord | 1.U
//       io.mem.req.valid := true.B
//       io.mem.req.bits.addr := objPAddr
//       io.mem.req.bits.cmd := M_XWR
//       io.mem.req.bits.data := statusWord
//       when(io.mem.resp.valid) {
//         // Translate TIB virtual address to physical address
//         tibVAddr := Cat(statusWord(63, 32), 0.U(32.W)) // 32 LSBs as TIB address
//         state := loadNumRefs
//       }
//     }

//     is(loadNumRefs) {
//       // Translate TIB address
//       io.tlb.req.valid := true.B
//       io.tlb.req.bits.vaddr := tibVAddr
//       when(io.tlb.resp.valid) {
//         tibPAddr := io.tlb.resp.bits.paddr
//         state := loadNumRefs
//       }
//       // Load number of outbound references
//       io.mem.req.valid := true.B
//       io.mem.req.bits.addr := tibPAddr
//       io.mem.req.bits.cmd := M_XRD
//       when(io.mem.resp.valid) {
//         numRefs := io.mem.resp.bits.data
//         refOffsetsAddr := tibPAddr + 8.U
//         objBasePAddr := objPAddr
//         refCounter := 0.U
//         state := processRefs
//       }
//     }

//     is(processRefs) {
//       // Check if there are references left to process
//       when(refCounter < numRefs) {
//         state := loadRefOffset
//       }.otherwise {
//         state := idle
//       }
//     }

//     is(loadRefOffset) {
//       // Load reference offset from memory
//       io.mem.req.valid := true.B
//       io.mem.req.bits.addr := refOffsetsAddr
//       io.mem.req.bits.cmd := M_XRD
//       when(io.mem.resp.valid) {
//         refOffset := io.mem.resp.bits.data
//         refOffsetsAddr := refOffsetsAddr + 8.U
//         state := outputRef
//       }
//     }

//     is(outputRef) {
//       // Calculate and output reference address
//       refAddr := objBasePAddr + refOffset
//       io.outboundRefAddrOut := refAddr
//       io.outputRefReady := true.B
//       refCounter := refCounter + 1.U
//       state := waitRefOffset
//     }

//     is(waitRefOffset) {
//       // Deassert output signal after one cycle
//       io.outputRefReady := false.B
//       state := processRefs
//     }
//   }
// }



class Marker(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val markQueueVAddrIn = Input(UInt(64.W))  // 64-bit virtual address from mark queue
    val markQueueValidIn = Input(Bool())      // Valid signal from mark queue
    val outboundRefAddrOut = Output(UInt(64.W)) // Outbound reference physical address
    val outputDoneProcessing = Output(Bool())   
    val outputRefReady = Output(Bool())    // Signal that a reference address is ready
    val mem = new HellaCacheIO                 // TileLink memory interface
  })

  // Define state machine states
  val idle :: translateAddr :: loadStatusWord :: setMarkBit :: loadNumRefs :: processRefs :: outputRef :: waitRefOffset :: loadRefOffset :: Nil = Enum(9)
  val state = RegInit(idle)

  // Internal Registers
  val objVAddr = Reg(UInt(64.W))             // Object virtual address
  val objPAddr = Reg(UInt(64.W))             // Object physical address
  val statusWord = Reg(UInt(64.W))           // Status word
  val tibVAddr = Reg(UInt(64.W))             // TIB virtual address
  val numRefs = Reg(UInt(64.W))              // Number of references
  val refCounter = Reg(UInt(32.W))           // Reference counter
  val refOffsetsAddr = Reg(UInt(64.W))       // Reference offsets address
  val refAddr = Reg(UInt(64.W))              // Reference address

  // Outputs Defaults
  io.outboundRefAddrOut := refAddr
  io.outputDoneProcessing := false.B
  io.outputRefReady := false.B

  // Memory and TLB control signals
  io.mem.req.valid := false.B
  io.mem.req.bits.cmd := M_XRD // Default read command, will be adjusted as necessary
  io.mem.req.bits.size := log2Ceil(8).U // 64-bit accesses
  io.mem.req.bits.addr := objPAddr

  val tlb = Module(new TLB(instruction = false, lgMaxSize = log2Ceil(8), nEntries = 32))
  tlb.io.req.valid := false.B
  tlb.io.req.bits.vaddr := objVAddr
  tlb.io.req.bits.size := log2Ceil(8).U
  tlb.io.req.bits.cmd := M_XRD

  switch(state) {
    is(idle) {
      io.outputDoneProcessing := true.B
      when(io.markQueueValidIn) {
        objVAddr := io.markQueueVAddrIn
        state := translateAddr
        io.outputDoneProcessing := false.B
      }
    }
    is(translateAddr) {
      tlb.io.req.valid := true.B
      tlb.io.req.bits.vaddr := objVAddr
      when(tlb.io.resp.valid) {
        objPAddr := tlb.io.resp.bits.paddr
        state := loadStatusWord
      }
    }
    is(loadStatusWord) {
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := objPAddr
      io.mem.req.bits.cmd := M_XRD
      when(io.mem.resp.valid) {
        statusWord := io.mem.resp.bits.data
        state := setMarkBit
      }
    }
    is(setMarkBit) {
      statusWord := statusWord | 1.U
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := objPAddr
      io.mem.req.bits.cmd := M_XWR
      io.mem.req.bits.data := statusWord
      when(io.mem.resp.valid) {
        tibVAddr := Cat(statusWord(63, 32), 0.U(32.W))
        state := loadNumRefs
      }
    }
    is(loadNumRefs) {
      tlb.io.req.valid := true.B
      tlb.io.req.bits.vaddr := tibVAddr
      when(tlb.io.resp.valid) {
        val tibPAddr = tlb.io.resp.bits.paddr
        io.mem.req.valid := true.B
        io.mem.req.bits.addr := tibPAddr
        io.mem.req.bits.cmd := M_XRD
        when(io.mem.resp.valid) {
          numRefs := io.mem.resp.bits.data
          refCounter := 0.U
          refOffsetsAddr := tibPAddr + 8.U
          state := processRefs
        }
      }
    }
    is(processRefs) {
      when(refCounter < numRefs) {
        io.mem.req.valid := true.B
        io.mem.req.bits.addr := refOffsetsAddr + (refCounter << 3)
        io.mem.req.bits.cmd := M_XRD
        when(io.mem.resp.valid) {
          refAddr := io.mem.resp.bits.data + objPAddr
          refCounter := refCounter + 1.U
          state := outputRef
        }
      } .otherwise {
        state := idle
      }
    }
    is(outputRef) {
      io.outputRefReady := true.B
      when(io.outputRefReady) {
        state := processRefs
      }
    }
  }
}