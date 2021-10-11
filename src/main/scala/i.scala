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

class Piper extends Module {

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val opcode = Input(UInt(2.W))
    val rdaddr = Input(UInt(8.W))
    val wraddr = Input(UInt(8.W))
    val out = Output(UInt(24.W))
  })

  val n = 20

  val enables = Helper.DelayLine(io.enable, n, tag="enables")
  val opcodes = Helper.DelayLine(io.opcode, n, tag="opcodes")
  val rdaddrs = Helper.DelayLine(io.rdaddr, n, tag="rdaddrs")
  val wraddrs = Helper.DelayLine(io.wraddr, n, tag="wraddrs")
  val data = Helper.DelayLineWire(UInt(24.W), n, tag="data")

  val m = SyncReadMem( 256, UInt(24.W))

  val rd_latency = 1
  val rd_stage = 2

  assert(rd_stage > rd_latency)

  val ex_stage = 2
  val wr_stage = 4
  val ou_stage = 5

  val read_data = m.read(rdaddrs(rd_stage-1-rd_latency), enables(rd_stage-1-rd_latency))
  // abnormal default cause when you don't want a pipe stage between rd and ex
  val ex_inp = WireInit(read_data)
  if (rd_stage < ex_stage) {
    data(rd_stage) := read_data
    ex_inp := data(ex_stage-1)
  }

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

  data(ex_stage) := alu(ex_inp, opcodes(ex_stage-1))


  when (enables(wr_stage-1)) {
    m.write( wraddrs(wr_stage-1), data(wr_stage-1))
  }

  io.out := data(ou_stage)

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
