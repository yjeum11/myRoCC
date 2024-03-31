package myRoCC

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip._
import freechips.rocketchip.diplomacy._

class WithmyRoCC extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    val myrocc = LazyModule.apply(new myRoCC(OpcodeSet.custom0)(p))
    myrocc
  })
})

class myRoCC(opcodes: OpcodeSet)
    (implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new myRoCCModule(this)
}

class myRoCCModule(outer: myRoCC)
    extends LazyRoCCModuleImp(outer) {
  val cmd = Queue(io.cmd)
  // The parts of the command are as follows
  // inst - the parts of the instruction itself
  //   opcode
  //   rd - destination register number
  //   rs1 - first source register number
  //   rs2 - second source register number
  //   funct
  //   xd - is the destination register being used?
  //   xs1 - is the first source register being used?
  //   xs2 - is the second source register being used?
  // rs1 - the value of source register 1
  // rs2 - the value of source register 2
  val result = cmd.bits.rs1 * cmd.bits.rs2
  cmd.ready := true.B
  io.busy := false.B
  io.resp.bits.rd := cmd.bits.inst.rd
  io.resp.bits.data := result
  io.resp.valid := cmd.valid
  io.interrupt := false.B
}
