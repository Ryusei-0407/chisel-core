package sw

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import common.Consts._

// addr -> Input for memory address
// inst -> Output for instruction data
// both are 32-bit (WORD_LEN.W = 32)
class ImemPortIo extends Bundle {
  val addr = Input(UInt(WORD_LEN.W))
  val inst = Output(UInt(WORD_LEN.W))
}

// Exchanging data between CPU and memory
class DmemPortIo extends Bundle {
  val addr = Input(UInt(WORD_LEN.W))
  val rdata = Output(UInt(WORD_LEN.W))
  // Signal for memory write
  val wen = Input(Bool())
  // Write in data for memory
  val wdata = Input(UInt(WORD_LEN.W))
}

class Memory extends Module {
  val io = IO(new Bundle {
    val imem = new ImemPortIo()
    val dmem = new DmemPortIo()
  })

  // Create register for 8bit * 16384 (= 16KB)
  // 1 address = 8bit <=> 4 address = 32bit
  val mem = Mem(16384, UInt(8.W))

  // Road memory data
  loadMemoryFromFile(mem, "src/hex/sw.hex")

  // 8bit * 4 = 32bit
  io.imem.inst := Cat(
    mem(io.imem.addr + 3.U(WORD_LEN.W)),
    mem(io.imem.addr + 2.U(WORD_LEN.W)),
    mem(io.imem.addr + 1.U(WORD_LEN.W)),
    mem(io.imem.addr)
  )

  io.dmem.rdata := Cat(
    mem(io.dmem.addr + 3.U(WORD_LEN.W)),
    mem(io.dmem.addr + 2.U(WORD_LEN.W)),
    mem(io.dmem.addr + 1.U(WORD_LEN.W)),
    mem(io.dmem.addr)
  )

  when(io.dmem.wen) {
    mem(io.dmem.addr) := io.dmem.wdata(7, 0)
    mem(io.dmem.addr + 1.U) := io.dmem.wdata(15, 8)
    mem(io.dmem.addr + 2.U) := io.dmem.wdata(23, 16)
    mem(io.dmem.addr + 3.U) := io.dmem.wdata(31, 24)
  }
}
