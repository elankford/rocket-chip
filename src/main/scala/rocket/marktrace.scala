import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._ // Importing Rocket Chip components for memory interfaces

class UInt64Queue(depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(64.W))) // Enqueue interface
    val deq = Decoupled(UInt(64.W))          // Dequeue interface
  })
  
  val queue = Module(new Queue(UInt(64.W), depth))
  queue.io.enq <> io.enq
  io.deq <> queue.io.deq
}

class MarkSweep(addrWidth: Int, queueDepth: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val GCbit = Input(Bool())       // Start garbage collection
    val loadMem = Input(Bool())     // Signal to load roots into mark queue
    val markready = Input(Bool())   // Ready to mark next address
    val queuesEmpty = Input(Bool()) // Indicates if both queues are empty

    // Memory interface (Rocket)
    val mem = new HellaCacheIO // Memory access interface

    // Control outputs
    val GCbitOut = Output(Bool())    // Output for GC status
    val loadMemOut = Output(Bool())  // Output for loading roots
  })

  // State machine states
  val sIdle :: sLoadRoots :: sMark :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // Instantiate mark queue
  val markQueue = Module(new UInt64Queue(queueDepth))

  // Internal signals and registers
  val GCbitReg = RegInit(false.B)
  val loadMemReg = RegInit(false.B)
  val rootAddrIndex = RegInit(0.U(addrWidth.W)) // Root address pointer
  val markData = Reg(UInt(64.W))                // Loaded data for marking
  val tibPointer = Reg(UInt(addrWidth.W))       // Points to the TIB in memory
  val numOutboundRefs = Reg(UInt(32.W))         // Number of outbound references in TIB
  val outboundIndex = RegInit(0.U(32.W))        // Iterator through outbound references

  val numRoots = 10.U // Example number of roots, could be parameterized

  // Output signals
  io.GCbitOut := GCbitReg
  io.loadMemOut := loadMemReg

  // Connect queue signals
  markQueue.io.enq.valid := false.B
  markQueue.io.enq.bits := 0.U
  markQueue.io.deq.ready := false.B

  // Memory interface signals
  io.mem.req.valid := false.B
  io.mem.req.bits.addr := 0.U
  io.mem.req.bits.cmd := M_XRD // Default command is read
  io.mem.req.bits.typ := MT_D  // Default memory type (64-bit word)
  io.mem.req.bits.data := 0.U
  io.mem.req.bits.phys := false.B

  switch(state) {
    is(sIdle) {
      when(io.GCbit && io.loadMem) {
        // Start GC and load roots
        GCbitReg := true.B
        loadMemReg := true.B
        state := sLoadRoots
      }
    }

    is(sLoadRoots) {
      when(loadMemReg) {
        // Load root addresses into the mark queue
        io.mem.req.valid := true.B
        io.mem.req.bits.addr := rootAddrIndex
        io.mem.req.bits.cmd := M_XRD

        when(io.mem.resp.valid) {
          markQueue.io.enq.valid := true.B
          markQueue.io.enq.bits := io.mem.resp.bits.data // Enqueue root data
          
          when(markQueue.io.enq.ready) {
            rootAddrIndex := rootAddrIndex + 1.U
            when(rootAddrIndex === numRoots) {
              // All roots loaded
              loadMemReg := false.B
              state := sMark
            }
          }
        }
      }
    }

    is(sMark) {
      when(GCbitReg) {
        when(io.markready) {
          when(markQueue.io.deq.valid) {
            markQueue.io.deq.ready := true.B
            val addrToProcess = markQueue.io.deq.bits

            // Request to read memory at `addrToProcess`
            io.mem.req.valid := true.B
            io.mem.req.bits.addr := addrToProcess
            io.mem.req.bits.cmd := M_XRD

            when(io.mem.resp.valid) {
              markData := io.mem.resp.bits.data
              // Set mark bit (assuming LSB is mark bit)
              markData := markData | 1.U

              // Write back with mark bit set
              io.mem.req.valid := true.B
              io.mem.req.bits.addr := addrToProcess
              io.mem.req.bits.cmd := M_XWR
              io.mem.req.bits.data := markData

              // Extract TIB pointer and number of references
              tibPointer := markData(63, 32) // Upper 32 bits as TIB pointer
              numOutboundRefs := markData(31, 0)
              outboundIndex := 0.U
            }

            // Process each outbound reference
            when(outboundIndex < numOutboundRefs) {
              // Calculate address for outbound reference in TIB
              val outboundAddr = tibPointer + (outboundIndex << 2)
              io.mem.req.valid := true.B
              io.mem.req.bits.addr := outboundAddr
              io.mem.req.bits.cmd := M_XRD

              when(io.mem.resp.valid) {
                // Push outbound reference to tracer queue (to be implemented)
                // ...
                outboundIndex := outboundIndex + 1.U
              }
            }

            when(outboundIndex === numOutboundRefs) {
              state := Mux(markQueue.io.deq.valid, sMark, sDone)
            }
          } .elsewhen(io.queuesEmpty) {
            // If both queues are empty, complete GC
            GCbitReg := false.B
            state := sDone
          }
        }
      }
    }

    is(sDone) {
      // GC completed, reset to idle
      state := sIdle
    }
  }
}
