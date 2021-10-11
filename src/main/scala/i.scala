package piper

import chisel3._
//import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation

import dotvisualizer.stage.DiagrammerStage

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
    val delayed = IndexedSeq.tabulate(n){ i => Reg( inp.cloneType).suggestName( f"${tag}_${i+1}%03xH")}
    ((IndexedSeq(inp) ++ delayed) zip delayed).foreach { case (i,o) => o := i }
    IndexedSeq(inp) ++ delayed
  }
}

class Piper  extends Module {

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val opcode = Input(UInt(2.W))
    val rdaddr = Input(UInt(8.W))
    val wraddr = Input(UInt(8.W))
    val out = Output(UInt(24.W))
  })

  val n = 40

  val enables = Helper.DelayLine(io.enable, n, tag="enables")
  val opcodes = Helper.DelayLine(io.opcode, n, tag="opcodes")
  val rdaddrs = Helper.DelayLine(io.rdaddr, n, tag="rdaddrs")
  val wraddrs = Helper.DelayLine(io.wraddr, n, tag="wraddrs")
  val data = Helper.DelayLineWire(UInt(24.W), n, tag="data")

  val rd_latency = 1
  assert(rd_latency == 1 || rd_latency == 0)
  val rd_stage = rd_latency + 1
  assert(rd_stage >= rd_latency + 1)
  val ex_stage = rd_stage + 3
  assert(ex_stage >= rd_stage)
  val wr_stage = ex_stage + 2
  assert(wr_stage >= ex_stage)
  val ou_stage = ex_stage + 0
  assert(ou_stage >= ex_stage)

  val m = if (rd_latency == 1) SyncReadMem( 256, io.out.cloneType) else Mem( 256, io.out.cloneType)

  val read_data = WireInit(data(0).cloneType, init=DontCare)
  when (enables(rd_stage-1-rd_latency)) {
    read_data := m.read(rdaddrs(rd_stage-1-rd_latency))
  }
  data(rd_stage) := read_data

  def alu(inp : UInt, op : UInt) : UInt = {
    val result = Wire(inp.cloneType)
    when (op === 0.U) {
      result := inp + 1.U
    } .elsewhen (op === 1.U) {
      result := inp - 1.U
    } .elsewhen (op === 2.U) {
      result := inp
    } .elsewhen (op === 3.U) {
      result := 0.U
    } .otherwise {
      result := DontCare
    }
    result
  }

  // abnormal case when you don't want a pipe stage between rd and ex
  val ex_data = alu((if (rd_stage < ex_stage) data(ex_stage-1) else read_data), opcodes(ex_stage-1))
  data(ex_stage) := ex_data

  when (enables(wr_stage-1)) {
    m.write( wraddrs(wr_stage-1), (if (ex_stage < wr_stage) data(wr_stage-1) else ex_data))
  }

  io.out := (if (ex_stage < ou_stage) data(ou_stage) else ex_data)

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
