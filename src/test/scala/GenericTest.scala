package test_helpers

import org.scalatest.{ Matchers, FlatSpec}

//import chisel3._
//import chisel3.util._
import chisel3.iotesters._

class GenericTest extends FlatSpec with Matchers {
  val optionsManager = new TesterOptionsManager {
    testerOptions = testerOptions.copy(
      backendName="treadle"
//      backendName="verilator"
//      backendName="vcs"
      ,isVerbose=true
//      ,moreVcsFlags=Seq("-sverilog -notice")
//      ,vcsCommandEdits="""s/\+vcs\+initreg\+random //"""
    )
  }
}
