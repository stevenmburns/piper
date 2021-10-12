package piper

import chisel3._
//import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation

import dotvisualizer.stage.DiagrammerStage

import scala.math.max

object Helper {
/*
wire     reg        reg        reg        reg

       +------+   +------+   +------+   +------+
r(0)---+ r(1) +---+ r(2) +---+ r(3) +---+ r(4) +---
       +------+   +------+   +------+   +------+

Assign to r(0) to set the input
Assign to r(i) to change the input to the flop (for i > 0) 
Read from r(9) to get the value of the input
Read from r(i) to get the value of the flop (for i > 0)
 */

  def DelayLineWire[T <: Data](proto : T, n: Int, tag : String) : IndexedSeq[T] = {
    val inp_wire = Wire(proto.cloneType).suggestName( f"${tag}_${0}%03xH")
    inp_wire := DontCare
    val delayed = IndexedSeq.tabulate(n){ i => Reg( proto.cloneType).suggestName( f"${tag}_${i+1}%03xH")}
    ((IndexedSeq(inp_wire) ++ delayed) zip delayed).foreach { case (i,o) => o := i }
    IndexedSeq(inp_wire) ++ delayed
  }
  def DelayLine[T <: Data](inp : T, n: Int, tag : String) : IndexedSeq[T] = {
    val result = DelayLineWire(inp.cloneType, n, tag)
    result(0) := inp
    result
  }
}

class Piper  extends Module {

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val opcode = Input(UInt(2.W))
    val rdaddr = Input(UInt(8.W))
    val wraddr = Input(UInt(8.W))
    val literal = Input(UInt(24.W))
    val out = Output(UInt(24.W))
  })

  val n = 40

  val enables  = Helper.DelayLine(io.enable,  n, tag="enables")
  val opcodes  = Helper.DelayLine(io.opcode,  n, tag="opcodes")
  val literals = Helper.DelayLine(io.literal, n, tag="literals")
  val rdaddrs  = Helper.DelayLine(io.rdaddr,  n, tag="rdaddrs")
  val wraddrs  = Helper.DelayLine(io.wraddr,  n, tag="wraddrs")

  val readdata = Helper.DelayLineWire(UInt(24.W), n, tag="readdata")
  val exdata   = Helper.DelayLineWire(UInt(24.W), n, tag="exdata")

  val rd_latency = 0
  assert(rd_latency == 1 || rd_latency == 0)
  val rd_stage = rd_latency + 0
  assert(rd_stage >= rd_latency)
  val ex_stage = rd_stage + 0
  assert(ex_stage >= rd_stage)
  val wr_stage = ex_stage + 0
  assert(wr_stage >= ex_stage)
  val ou_stage = ex_stage + 0
  assert(ou_stage >= ex_stage)

  val m = if (rd_latency == 1) SyncReadMem( 256, io.out.cloneType) else Mem( 256, io.out.cloneType)

  when (enables(max(0, rd_stage-1-rd_latency))) {
    readdata(rd_stage) := m.read(rdaddrs(max(0, rd_stage-1-rd_latency)))
  }

  def alu(inp : UInt, op : UInt, lit : UInt) : UInt = {
    val result = Wire(inp.cloneType)
    when (op === 0.U) {
      result := inp + 1.U
    } .elsewhen (op === 1.U) {
      result := inp - 1.U
    } .elsewhen (op === 2.U) {
      result := inp
    } .elsewhen (op === 3.U) {
      result := lit
    } .otherwise {
      result := DontCare
    }
    result
  }

  exdata(ex_stage) := alu(readdata(max(0, max(ex_stage-1, rd_stage))), opcodes(max(0, ex_stage-1)), literals(max(0, ex_stage-1)))

  when (enables(max(0, wr_stage-1))) {
    m.write( wraddrs(max(0, wr_stage-1)), exdata(max(0, max(wr_stage-1, ex_stage))))
  }

  io.out := exdata(max(0, max(ou_stage-1, ex_stage)))

}

object Piper1 {
  def main(args: Array[String]): Unit = {
    val targetDir = "test_run_dir/piper1"
    (new DiagrammerStage).execute(
      Array("-td", targetDir),
      Seq(ChiselGeneratorAnnotation( () => new Piper))
    )
  }
}
