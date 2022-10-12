// This is borrowed from rocket, and modified to be hardcoded to 32b.
// I will leave it as an excercise to the reader to make a parameterizable ALU
// that doesn't generate extra hardware for 32b. I also didn't carefully
// consider the function encodings. - Chris
package sodor.rv64

import chisel3._
import chisel3.util._

import sodor.common._
import Constants._
import sodor.rv64.funcs

object ALU
{
  // TODO is this the optimal encoding?
  val SZ_ALU_FN = 4
  val ALU_X    = 0.U // TODO use a more optimal decode table, which uses "???" format
  val ALU_ADD  = 0.U
  val ALU_SLL  = 1.U
  val ALU_XOR  = 4.U
  val ALU_OR   = 6.U
  val ALU_AND  = 7.U
  val ALU_SRL  = 5.U
  val ALU_SUB  = 10.U
  val ALU_SRA  = 11.U
  val ALU_SLT  = 12.U
  val ALU_SLTU = 14.U
  val ALU_COPY1= 8.U

  // ALU_*32 should be connected to {op32, fn}
  val ALU_ADD32 = Cat(1.U, ALU_ADD(3,0))
  val ALU_SLL32 = Cat(1.U, ALU_SLL(3,0))
  val ALU_SRL32 = Cat(1.U, ALU_SRL(3,0))
  val ALU_SUB32 = Cat(1.U, ALU_SUB(3,0))
  val ALU_SRA32 = Cat(1.U, ALU_SRA(3,0))

  def isSub(cmd: UInt) = cmd(3)
  def isSLTU(cmd: UInt) = cmd(1)
}
import ALU._

class ALUIO(implicit val conf: SodorCoreParams) extends Bundle {
  val fn = Input(UInt(SZ_ALU_FN.W))
  val op32 = Input(Bool())
  val in2 = Input(UInt(conf.xprlen.W))
  val in1 = Input(UInt(conf.xprlen.W))
  val out = Output(UInt(conf.xprlen.W))
  val adder_out = Output(UInt(conf.xprlen.W))
}

class ALU(implicit val conf: SodorCoreParams) extends Module
{
  val io = IO(new ALUIO)

  val msb = conf.xprlen-1

  // ADD, SUB
  val sum = io.in1 + Mux(isSub(io.fn), -io.in2, io.in2)
  // ADDW, SUBW
  val sum32 = funcs.sign_ext(io.in1(31,0) + Mux(isSub(io.fn), -(io.in2(31,0)), io.in2(31,0)), out_width = 64)
  val final_sum = Mux(io.op32, sum32, sum)

  // SLT, SLTU
  val less  =  Mux(io.in1(msb) === io.in2(msb), sum(msb),
              Mux(isSLTU(io.fn), io.in2(msb), io.in1(msb)))

  // SLL, SRL, SRA
  val shamt = io.in2(5,0)
  val shin_r = io.in1(63,0)
  val shin = Mux(io.fn === ALU_SRL  || io.fn === ALU_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(msb), shin) >> shamt)(msb,0)
  val shout_l = Reverse(shout_r)

  // SLLW, SRLW, SRAW
  val shamt32 = io.in2(4,0)
  val shin_r32 = io.in1(31,0)
  val shin32 = Mux(io.fn === ALU_SRL || io.fn === ALU_SRA, shin_r32, Reverse(shin_r32))
  val shout_r32 = funcs.sign_ext((Cat(isSub(io.fn) & shin32(31), shin32) >> shamt32)(31,0), out_width = 64)
  val shout_l32 = Reverse(shout_r32)

  val final_shout_r = Mux(io.op32, shout_r32, shout_r)
  val final_shout_l = Mux(io.op32, shout_l32, shout_l)

  // i prefer MuxCase because it looks cooler
  // default is for ALU_COPY1
  val bitwise_logic =
    MuxCase(io.in1, Array(
      (io.fn === ALU_AND) ->  (io.in1 & io.in2),
      (io.fn === ALU_OR) ->   (io.in1 | io.in2),
      (io.fn === ALU_XOR) ->  (io.in1 ^ io.in2),
    ))

  val out_xpr_length =
    MuxCase(bitwise_logic, Array(
      (io.fn === ALU_ADD || io.fn === ALU_SUB) ->   final_sum,
      (io.fn === ALU_SLT || io.fn === ALU_SLTU) ->  less,
      (io.fn === ALU_SRL || io.fn === ALU_SRA) ->   final_shout_r,
      (io.fn === ALU_SLL) ->                        final_shout_l
    ))

  io.out := out_xpr_length
  io.adder_out := final_sum
}

object convertALU extends App {
  val param = new SodorCoreParams()
  (new chisel3.stage.ChiselStage().emitVerilog(new ALU()(param)))
}

