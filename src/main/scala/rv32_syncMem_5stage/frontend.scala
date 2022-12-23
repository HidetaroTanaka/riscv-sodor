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
   val pc   = UInt(xprlen.W)
   val inst = UInt(xprlen.W)  // only support 32b insts for now

}

class FrontEndDebug(xprlen: Int) extends Bundle
{
   val if_pc   = Output(UInt(xprlen.W))
   val if_inst = Output(UInt(xprlen.W))
}

class FrontEndCpuIO(implicit val conf: SodorCoreParams) extends Bundle
{
   val req = Flipped(new ValidIO(new FrontEndReq(conf.xprlen)))
   // predicted pc in ID stage
   // jal prediction will be always correct, but any branch and jalr prediction might be incorrect
   val pc_predict = Flipped(new ValidIO(new FrontEndReq(conf.xprlen)))
   // calculated pc in EX stage.
   // if this is different from pc_predict, then core should raise resp.valid
   // resp.valid is true means that pc prediction miss
   val resp = new DecoupledIO(new FrontEndResp(conf.xprlen))

   val debug = new FrontEndDebug(conf.xprlen)

   // not used wtf
   // val imiss = Output(Bool())
   // Flush the entire pipeline upon exception, including exe stage
   val exe_kill = Input(Bool())
}


class FrontEnd(implicit val conf: SodorCoreParams) extends Module
{
   val io = IO(new FrontEndIO)
   io := DontCare

   //**********************************
   // Pipeline State Registers
   val if_reg_pc     = RegInit(io.reset_vector - 4.U)

   //**********************************
   // Next PC Stage (if we can call it that)
   val if_pc_next = Wire(UInt(conf.xprlen.W))
   val if_val_next = Wire(Bool()) // for now, always true. But instruction
                                // buffers, etc., could make that not the case.

   val if_pc_plus4 = (if_reg_pc + 4.asUInt(conf.xprlen.W))

   // Redirect handling
   val if_redirected = RegInit(false.B)
   val if_redirected_pc = Reg(UInt(conf.xprlen.W))

   // Instruction buffer
   val if_buffer_in = Wire(new DecoupledIO(new MemResp(conf.xprlen)))
   if_buffer_in.valid := io.imem.resp.valid 
   if_buffer_in.bits := io.imem.resp.bits
   if_val_next := io.cpu.resp.ready || (if_buffer_in.ready && !io.imem.resp.valid) // If the incoming inst goes to buffer, don't send the next req
   assert(if_buffer_in.ready || !if_buffer_in.valid, "Inst buffer overflow")
   val if_buffer_out = Queue(if_buffer_in, entries = 1, pipe = false, flow = true)

   // stall IF/EXE if backend not ready
   // デフォルトではpc_nextは現在のpc+4
   if_pc_next := if_pc_plus4
   // if pc prediction is valid, next pc should be predicted pc
   // (this pc is predicted in ID stage)
   when (io.cpu.pc_predict.valid) {
      if_pc_next := io.cpu.pc_predict.bits.pc
   }
   // if cpu requests new (and correct) pc from EX stage, next pc should be this pc.
   // true pc calculated in EX stage
   when (io.cpu.req.valid)
   {
      // datapath is redirecting the PC stream (misspeculation)
      // データパスがPCを更新する（分岐予測ミスにより）
      if_redirected := true.B
      if_redirected_pc := io.cpu.req.bits.pc
   }
   // if frontend redirects pc from EX stage, next pc should be the pc.
   when (if_redirected)
   {
      if_pc_next := if_redirected_pc
   }
   /*
    * Sodorの命令メモリ仕様
    * 1. 命令メモリがbusyで無ければ，imem.req.readyはtrue
    * 2. imem.req.validが真であれば，次のクロックサイクルまたはそれ以降でimem.respからデータが出てくる（正しいデータが出ている時にtrue）
    * 3. アクセスミス等があればimem.resp.validをfalseにし，正しいデータが出てくる時にtrueにする
    * 4. addrが変わらず，imem.resp.validがtrueであれば出力を変えない
    *
    */

   // Go to next PC if both CPU and imem are ready, and the memory response for the current PC already presents
   val if_reg_pc_responded = RegInit(false.B)

   // 前のサイクルで命令メモリが応答しておりコアが待機中である，または今のサイクルで命令メモリが応答した
   val if_pc_responsed = if_reg_pc_responded || io.imem.resp.valid

   // whether if_reg_pc can be updated or not
   // pc is updated only if pc responded and cpu is ready to get next frontend resp
   val if_pc_update = if_pc_responsed && io.cpu.resp.ready && io.imem.req.ready

   // PCレジスタにif_pc_nextを入れる
   when (if_pc_update)
   {
      if_reg_pc_responded := false.B
      if_reg_pc    := if_pc_next
      when (!io.cpu.req.valid)
      {
         if_redirected := false.B
      }
   }
   // 上の条件が満たされていないが，命令メモリからの応答がある場合
   .elsewhen (io.imem.resp.valid)
   {
      if_reg_pc_responded := true.B
   }

   // set up outputs to the instruction memory
   io.imem.req.bits.addr := if_pc_next
   io.imem.req.valid     := if_val_next && !io.cpu.req.valid
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
