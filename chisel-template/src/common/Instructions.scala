package common

import chisel3._
import chisel3.util._

object Instructions {

  // LW -> I Form
  val LW = BitPat("b?????????????????010?????0000011")
  // SW -> S Form
  val SW = BitPat("b?????????????????010?????0100011")

  // ADD
  val ADD = BitPat("b0000000??????????000?????0110011")
  // SUB
  val SUB = BitPat("b0100000??????????000?????0110011")
  // ADDI
  val ADDI = BitPat("b?????????????????000?????0010011")

  // AND
  val AND = BitPat("b0000000??????????111?????0110011")
  // OR
  val OR = BitPat("b0000000??????????110?????0110011")
  // XOR
  val XOR = BitPat("b0000000??????????100?????0110011")
  // ANDI
  val ANDI = BitPat("b?????????????????111?????0010011")
  // ORI
  val ORI = BitPat("b?????????????????110?????0010011")
  // XORI
  val XORI = BitPat("b?????????????????100?????0010011")
}
