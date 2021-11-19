package lw

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Core extends Module {
  val io = IO(new Bundle {
      val imem = Flipped(new ImemPortIo())
      val dmem = Flipped(new DmemPortIo())
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
   * Instruction Decode (ID) Stage
   */

  // rs1 register -> 15-19bit
  val rs1_addr = inst(19, 15)
  // rs2 register -> 20-24bit
  val rs2_addr = inst(24, 20)
  // rd register -> 7-11bit
  val wb_addr = inst(11, 7)
  
  // Reading data from a register
  val rs1_data = Mux((rs1_addr =/= 0.U(WORD_LEN.U)), regfile(rs1_addr), 0.U(WORD_LEN.W))
  val rs2_data = Mux((rs2_addr =/= 0.U(WORD_LEN.U)), regfile(rs2_addr), 0.U(WORD_LEN.W))

  // Sign extension of offset
  val imm_i = inst(31, 20)
  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)

  /*
   * Execute (EX) Stage
   */

  // Calculation of memory address
  val alu_out = MuxCase(0.U(WORD_LEN.W), Seq(
    (inst === LW) -> (rs1_data + imm_i_sext)
  ))

  /*
   * Memory Access Stage
   */

  io.dmem.addr := alu_out

  /*
   * Write Back (WB) Stage
   */

  val wb_data = io.dmem.rdata
  when(inst === LW) {
    regfile(wb_addr) := wb_data
  }

  /*
   * Debug
   */

  // If exit signal is "14131211", then return true.B
  io.exit := (inst === 0x14131211.U(WORD_LEN.W))
  printf(p"rc_reg    :0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst      :0x${Hexadecimal(inst)}\n")
  printf(p"rs1_addr  :$rs1_addr\n")
  printf(p"rs2_addr  :$rs2_addr\n")
  printf(p"wb_addr   :$wb_addr\n")
  printf(p"rs1_data  :0x${Hexadecimal(rs1_data)}\n")
  printf(p"rs2_data  :0x${Hexadecimal(rs2_data)}\n")
  printf(p"wb_data   :0x${Hexadecimal(wb_data)}\n")
  printf(p"dmem.addr :${io.dmem.addr}\n")
  printf("----------------------------------\n")
}
