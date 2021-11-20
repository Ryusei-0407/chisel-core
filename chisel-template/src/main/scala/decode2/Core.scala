package decode2

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

  /*
   * LW Instruction -> I Form
   *
   * --------------------------------------------------
   * |    31-20    | 19-15 | 14-12  |  11-7  |  6-0   |
   * | imm_i[11:0] |  rs1  | funct3 | rd(wb) | opcode |
   * --------------------------------------------------
   *
   */
  // Sign extension of offset
  val imm_i = inst(31, 20)
  // Fill the upper 20 bits with the most significant bits of imm_i
  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)

  /*
   * SW Instruction -> S Form
   *
   * -------------------------------------------------------------
   * |    31-25    | 24-20 | 19-15 | 14-12  |    11-7   |  6-0   |
   * | imm_s[11:5] |  rs2  |  rs1  | funct3 | imm_s[4:0]| opcode |
   * -------------------------------------------------------------
   *
   */
  // Sign extension of offset
  val imm_s = Cat(inst(31, 25), inst(11, 7))
  // Fill the upper 20 bits with the most significant bits of imm_s
  val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)

  /*
   * Execute (EX) Stage
   */

  // MEN_X -> Not write in memory
  // MEN_S -> Write in memory
  val csignals = ListLookup(inst, List(ALU_X, OP1_RS1, OP2_RS2, MEN_X),
    Array(
      LW -> List(ALU_ADD, OP1_RS1, OP1_IMI, MEN_X),
      SW -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_S),
      ADD -> List(ALU_ADD, OP1_RS1, OP2_RS2, MEN_X),
      ADDI -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X),
      SUB -> List(ALU_SUB, OP1_RS1, OP2_RS2, MEN_X),
      AND -> List(ALU_AND, OP1_RS1, OP2_RS2, MEN_X),
      OR -> List(ALU_OR, OP1_RS1, OP2_RS2, MEN_X),
      XOR -> List(ALU_XOR, OP1_RS1, OP2_RS2, MEN_X),
      ANDI -> List(ALU_AND, OP1_RS1, OP2_IMI, MEN_X),
      ORI -> List(ALU_OR, OP1_RS1, OP2_IMI, MEN_X),
      XORI -> List(ALU_XOR, OP1_RS1, OP2_IMI, MEN_X)
    )
  )
  val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wen :: wb_sel :: Nil = csignals

  val op1_data = MuxCase(0.U(WORD_LEN.W), Seq(
    (op1_sel === OP1_RS1) -> rs1_data,
  ))

  val op2_data = MuxCase(0.U(WORD_LEN.W), Seq(
    (op2_sel === OP2_RS2) -> rs2_data,
    (op2_sel === OP2_IMI) -> imm_i_sext,
    (op2_sel === OP2_IMS) -> imm_s_sext
  ))

  val alu_out = MuxCase(0.U(WORD_LEN.W), Seq(
    (exe_fun === ALU_ADD) -> (op1_data + op2_data), // ADD, ADDI
    (exe_fun === ALU_SUB) -> (op1_data - op2_data), // SUB
    (exe_fun === ALU_AND) -> (op1_data & op2_data), // AND, ANDI
    (exe_fun === ALU_OR)  -> (op1_data | op2_data), // OR, ORI
    (exe_fun === ALU_XOR) -> (op1_data ^ op2_data), // XOR, XORI
  ))

  /*
   * Memory Access Stage
   */

  io.dmem.addr := alu_out
  io.dmem.wen := mem_wen
  io.dmem.wdata := rs2_data

  /*
   * Write Back (WB) Stage
   */

  val wb_data = MuxCase(alu_out, Seq(
    (wb_sel === WB_MEM) -> io.dmem.rdata
  ))

  when(rf_wen === REN_S) {
    regfile(wb_addr) := wb_data
  }

  /*
   * Debug
   */

  // If exit signal is "00602823", then return true.B
  io.exit := (inst === 0x00602823.U(WORD_LEN.W))
  printf(p"rc_reg     :0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst       :0x${Hexadecimal(inst)}\n")
  printf(p"rs1_addr   :$rs1_addr\n")
  printf(p"rs2_addr   :$rs2_addr\n")
  printf(p"wb_addr    :$wb_addr\n")
  printf(p"rs1_data   :0x${Hexadecimal(rs1_data)}\n")
  printf(p"rs2_data   :0x${Hexadecimal(rs2_data)}\n")
  printf(p"wb_data    :0x${Hexadecimal(wb_data)}\n")
  printf(p"dmem.addr  :${io.dmem.addr}\n")
  printf(p"dmem.wen   :${io.dmem.wen}\n")
  printf(p"dmem.wdata :0x${Hexadecimal(io.dmem.wdata)}\n")
  printf("----------------------------------\n")
}
