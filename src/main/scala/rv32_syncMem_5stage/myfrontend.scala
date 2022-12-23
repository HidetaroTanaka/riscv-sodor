//**************************************************************************
// RISCV Processor Front-end
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Jn 29
//
// Handles the fetching of instructions.
//
// The front-end will go into cruise-control and fetch whichever instrutions it
// feels like (probably just PC+4 for this simple pipeline), and it's the job
// of the datapath to assert "valid" on the "io.req" bundle when it wants to
// redirect the fetch PC.  Otherwise, io.req.valid is disasserted.

// There are a number of games we can play with the front-end.
//    - It can fetch doublewords, thus only using the memory port on every
//    other cycle.
//    - It can have a fetch buffer (combined with dw fetch) to hide memory port
//    hazards.
//     - It can use a stall cache (Krste Asanovic paper), which holds a cache
//     of previously stalled instructions.
//
// None of the above are implemented - I leave them as an excercise to the
// reader for now...


package sodor.rv32_syncMem_5stage

import chisel3._
import chisel3.util._
import Constants._
import sodor.common._


class FrontEndIO(implicit val conf: SodorCoreParams) extends Bundle
{
   val cpu  = new FrontEndCpuIO
   val imem = new MemPortIo(conf.xprlen)

   val reset_vector = Input(UInt(conf.xprlen.W))

}


class FrontEndReq(xprlen: Int) extends Bundle
{
   val pc   = UInt(xprlen.W)

}


class FrontEndResp(xprlen: Int) extends Bundle
{
   val pc      = UInt(xprlen.W)
   val pred_pc = UInt(xprlen.W)
   val inst    = UInt(xprlen.W)  // only support 32b insts for now

}

class FrontEndDebug(xprlen: Int) extends Bundle
{
   val if_pc   = Output(UInt(xprlen.W))
   val if_inst = Output(UInt(xprlen.W))
}

class FrontEndCpuIO(implicit val conf: SodorCoreParams) extends Bundle
{
   val req = Flipped(new ValidIO(new FrontEndReq(conf.xprlen)))

   // 分岐命令がコミットした際に，予測結果と異なっていた場合に，正しい分岐先pcとvalidが入力される
   val resp = new DecoupledIO(new FrontEndResp(conf.xprlen))

   val debug = new FrontEndDebug(conf.xprlen)

   // Flush the entire pipeline upon exception, including exe stage
   // 例外や割り込み等？
   val exe_kill = Input(Bool())
}

class BranchPredictorReq(xprlen: Int) extends Bundle {
   val pc   = UInt(xprlen.W)
   val inst = UInt(xprlen.W)
}

class BranchPredictorResp(xprlen: Int) extends Bundle {
   val branch_target = UInt(xprlen.W)
}

class BranchPredictorIO(implicit val conf: SodorCoreParams) extends Bundle {
   val req = Flipped(new ValidIO(new BranchPredictorReq(conf.xprlen)))
   val resp = new ValidIO(new BranchPredictorResp(conf.xprlen))
}

/**
  * 静的分岐予測
  * 後方で成立、前方で成立しない
  * @param conf
  */
class BranchPredictor(implicit val conf: SodorCoreParams) extends Module {
   val io = IO(new BranchPredictorIO)

   // do something according to Table 2.1 of riscv-spec

   // とりまjalの場合にpush、retの場合にpop
   val RAS = Reg(Vec(16, UInt(conf.xprlen.W)))
   val RAS_pointer = Reg(UInt(4.W))

   def JAL     = BitPat("b?????????????????????????1101111")
   def JALR    = BitPat("b?????????????????????????1100111")
   def BRANCH  = BitPat("b?????????????????????????1100011")

   def sext(bits: UInt): UInt = {
      val msb = bits(bits.getWidth-1)
      Cat(Fill(conf.xprlen-bits.getWidth, msb), bits)
   }

   val inst = io.req.bits.inst
   val imm_jal = sext(Cat(inst(31), inst(19,12), inst(20), inst(30,21), 0.U(1.W)))
   val imm_jalr = sext(inst(31,20)) + RAS(RAS_pointer)
   val imm_branch = sext(Cat(inst(31), inst(7), inst(30,25), inst(11,8), 0.U(1.W)))

   val branch_target = MuxLookup(inst, imm_branch,
      Seq(
         JAL      -> imm_jal,
         JALR     -> imm_jalr,
         BRANCH   -> imm_branch
      )
   )

   io.resp.bits := branch_target
   io.resp.valid := branch_target > io.req.bits.pc
}

class FrontEnd(implicit val conf: SodorCoreParams) extends Module
{
   val io = IO(new FrontEndIO)
   io := DontCare

   val pc_reg = Reg(UInt(conf.xprlen.W))

   val core_valid = RegInit(reset)

   val bp = Module(new BranchPredictor())

   val next_pc = Wire(UInt(conf.xprlen.W))
   next_pc := MuxCase(pc_reg,
      Array(
         io.cpu.req.valid -> io.cpu.req.bits,
         (io.cpu.resp.ready && io.imem.req.ready) -> (pc_reg + 4.U(conf.xprlen.W)),
         bp.io.resp.valid -> bp.io.resp.bits.branch_target
      )
   )

   // set up outputs to the instruction memory
   io.imem.req.bits.addr := ???
   io.imem.req.valid     := ???
   io.imem.req.bits.fcn  := M_XRD
   io.imem.req.bits.typ  := MT_WU

   //**********************************
   // Inst Fetch/Return Stage
   val counter = RegInit(0.U(2.W))
   counter := Mux(counter === 2.U, counter, counter + 1.U)

   if_buffer_out.ready := io.cpu.resp.ready
   // パイプラインフラッシュ，分岐予測ミス
   when (io.cpu.exe_kill || io.cpu.req.valid)
   {
      io.cpu.resp.valid := false.B
   }
   .elsewhen (io.cpu.resp.ready)
   {
      io.cpu.resp.valid := if_buffer_out.valid && !io.cpu.req.valid && !if_redirected && (counter === 2.U)
   }
   io.cpu.resp.bits.pc    := if_reg_pc
   io.cpu.resp.bits.inst  := if_buffer_out.bits.data

   //**********************************
   // only used for debugging
   io.cpu.debug.if_pc := if_reg_pc
   io.cpu.debug.if_inst := io.imem.resp.bits.data
}

object frontendConverter extends App {
   val param = SodorCoreParams()
   (new chisel3.stage.ChiselStage).emitVerilog(new FrontEnd()(param))
}
