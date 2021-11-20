package sw

import chisel3._
import org.scalatest._
import chiseltest._

class HexTest extends FlatSpec with ChiselScalatestTester {
  "mycpu" should "work through hex" in {
    test(new Top) { c =>
      // ~.peek() -> get value from ~(signal name), return as signal type
      // ~.litToBoolean -> convert to boolean same exit signal for Chisel
      while (!c.io.exit.peek().litToBoolean) {
        // ~.step() -> step through the circuit
        // If exit signale is false.B, then step a time
        c.clock.step(1)
      }
    }
  }
}
