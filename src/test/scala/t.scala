
package piper

//import chisel3._
//import chisel3.util._
import chisel3.iotesters._
import test_helpers._

class PiperTester( factory: () => Piper) extends GenericTest {
  behavior of s"Piper"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        poke( c.io.enable, 0)
        step(10)
        poke( c.io.enable, 1)
        poke( c.io.opcode, 3)
        for { i <- 0 until 8} {
          poke( c.io.literal, i)
          poke( c.io.wraddr, i)
          step(1)
        }
        poke( c.io.opcode, 0)
        val m = 7
        for { _ <- 0 until m} {
          for { i <- 0 until 8} {
            poke( c.io.rdaddr, i)
            poke( c.io.wraddr, i)
            step(1)
          }
        }
        poke( c.io.opcode, 2)
        for { i <- 0 until 8+c.ou_stage} {
          poke( c.io.rdaddr, i % 8)
          step(1)
          if (i >= c.ou_stage) {
            expect(c.io.out, m+i-c.ou_stage)
          }
        }
      }
    } should be (true)
  }
}

class PiperTest extends PiperTester( () => new Piper)
