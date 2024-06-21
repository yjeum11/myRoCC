package myRoCCCarry

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip._
import freechips.rocketchip.diplomacy._

class WithmyRoCCCarry extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    val myrocc = LazyModule.apply(new myRoCCCarry(OpcodeSet.all)(p))
    myrocc
  })
})

class myRoCCCarry(opcodes: OpcodeSet)
    (implicit p: Parameters) extends LazyRoCC(opcodes, usesFPU=true) {
  override lazy val module = new myRoCCCarryModule(this)
}

class myRoCCCarryModule(outer: myRoCCCarry)
    extends LazyRoCCModuleImp(outer) {
  //val cmd = Queue(io.cmd)
  //
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
  //
  // Idea: we only need adcqc + 2 3-bit fields to select cin and cout
  // conditional execution also needs 2 3-bit fields to select cout and ctrl
  // we also need cadcq
  // Carry clear instruction
  //
  // 7 bits of funct. 
  // funct[6] bit is for add/sub, funct[5:3] is flag1, funct[2:0] is flag2
  //
  // custom0 - 0 adcqc (unconditional cin and cout)
  //         - 1 sbcqc
  // custom1 - 0 caddqc (conditional cout)
  //         - 1 csubqc
  // custom2 - 0 cadcq (conditional cin)
  //         - 1 csbcq
  //         still need not conditional. Inc/decrement
  //         Set carry flag. I-type instruction?
  // custom3 - read/write flags
  //           MSB 1 read, 0 write
  //           bottom 3 bits is flag
  //
  val opcode = io.cmd.bits.inst.opcode

  val cin = Wire(Bool())
  cin := false.B
  val cin_index = io.cmd.bits.inst.funct(5, 3)
  val cout_index = io.cmd.bits.inst.funct(2, 0)
  val addsub = io.cmd.bits.inst.funct(6)

  val result = Wire(UInt(65.W))
  result := 0.U
  val cout = result(64)
  val sum = result(63, 0)

  val regs = RegInit(VecInit(Seq.fill(8)(false.B)))

  val dont_resp = Wire(Bool())
  dont_resp := false.B

  when (opcode === "b0001011".U) { // custom0
    when (addsub === 1.U) {
      result := io.cmd.bits.rs1 -& io.cmd.bits.rs2 - cin
    } .otherwise {
      result := io.cmd.bits.rs1 +& io.cmd.bits.rs2 + cin
    }

    when (io.cmd.fire) {
      when (cout_index =/= 0.U) {
        regs(cout_index) := cout
      }
    } 
    when (cin_index =/= 0.U) {
      cin := regs(cin_index)
    } 

    io.resp.bits.data := sum

  } .elsewhen (opcode === "b0101011".U) { //custom1
    // conditional cout
    val cond = regs(cin_index)
    
    when (io.cmd.fire) {
      when (cout_index =/= 0.U) {
        regs(cout_index) := Mux(cond, cout, false.B);
      }
      when (addsub === 1.U) {
        result := Mux(cond, io.cmd.bits.rs1 -& io.cmd.bits.rs2, io.cmd.bits.rs1)
      } .otherwise {
        result := Mux(cond, io.cmd.bits.rs1 +& io.cmd.bits.rs2, io.cmd.bits.rs1)
      }
    } 
    io.resp.bits.data := sum
    
  } .elsewhen (opcode === "b1011011".U) { //custom2
    // conditional cin
    //
    val cond = regs(cout_index)

    when (cin_index =/= 0.U) {
      cin := regs(cin_index)
    } 
    
    when (io.cmd.fire) {
      when (addsub === 1.U) {
        result := Mux(cond, io.cmd.bits.rs1 -& io.cmd.bits.rs2 + cin, io.cmd.bits.rs1)
      } .otherwise {
        result := Mux(cond, io.cmd.bits.rs1 +& io.cmd.bits.rs2 - cin, io.cmd.bits.rs1)
      }
    } 
    io.resp.bits.data := sum
  } .elsewhen (opcode === "b1111011".U) { //custom3
    when (addsub === 1.U) { // read from carry
      io.resp.bits.data := regs(cout_index)
    } .otherwise {
      // don't respond at all
      dont_resp := true.B
      io.resp.bits.data := 0.U
      regs(cout_index) := io.cmd.bits.rs1(0)
    }
  }

  io.cmd.ready := true.B
  io.busy := false.B
  // boom doesn't commit properly
  io.resp.bits.rd := io.cmd.bits.inst.rd
  io.resp.valid := io.cmd.valid && (~dont_resp)
  io.interrupt := false.B
}
