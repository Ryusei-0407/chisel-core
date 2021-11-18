package fetch

import chisel3._
import chisel3.util._
import common.Consts._

class Core extends Module {
  val io = IO(new Bundle {
      val imem = Flipped(new ImemPortIo())
      val exit = Output(Bool())
  })

  // Create 32bit (WORD_LEN.W = 32) * 32 registers
  val regfile = Mem(32, UInt(WORD_LEN.W))

  /*
   * Instruction Fetch (IF) Stage
   */

  // Create PC register for initial value is 0 (START_ADDR = 0)
  // Count up PC by 4 every cycle (4 bytes)
  val pc_reg = RegInit(START_ADDR)
  pc_reg := pc_reg + 4.U(WORD_LEN.W)

  // addr (Output) <=> re_reg
  // inst (Input) <=> inst
  io.imem.addr := pc_reg
  val inst = io.imem.inst

  /*
   * Debug
   */
  // If exit signal is "34333231", then return true.B
  io.exit := (inst === 0x34333231.U(WORD_LEN.W))
  printf(p"rc_reg    :0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst      :0x${Hexadecimal(inst)}\n")
  printf("----------------------------------\n")
}
