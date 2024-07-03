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
  val opcode = io.cmd.bits.inst.opcode

  val cin_index = io.cmd.bits.inst.funct(3, 2)
  val cout_index = io.cmd.bits.inst.funct(1, 0)

  val regs = RegInit(VecInit(Seq.fill(4)(false.B)))

  val funct7 = io.cmd.bits.inst.funct

  val dont_resp = Wire(Bool())
  dont_resp := false.B

  val sum = Wire(UInt(65.W))
  sum := 0.U

  io.resp.bits.data := 0.U

  when (opcode === "b0001011".U) { // custom0
    dont_resp := false.B
    when (funct7(6) === 0.U) { // addq
      when (funct7(5, 4) === 0.U) {
        sum := io.cmd.bits.rs1 +& io.cmd.bits.rs2
      } .elsewhen (funct7(5, 4) === 1.U) { // addqc
        sum := io.cmd.bits.rs1 +& io.cmd.bits.rs2
        when (io.cmd.fire) {
          regs(cout_index) := sum(64)
        }
      } .elsewhen (funct7(5, 4) === 2.U) { // adcq
        sum := io.cmd.bits.rs1 +& io.cmd.bits.rs2 +& regs(cin_index)
      } .otherwise { // adcqc
        sum := io.cmd.bits.rs1 +& io.cmd.bits.rs2 +& regs(cin_index)
        when (io.cmd.fire) {
          regs(cout_index) := sum(64)
        }
      }
    } .otherwise {
      when (funct7(5, 4) === 0.U) {
        sum := io.cmd.bits.rs1 -& io.cmd.bits.rs2
      } .elsewhen (funct7(5, 4) === 1.U) { // subqc
        sum := io.cmd.bits.rs1 -& io.cmd.bits.rs2
        when (io.cmd.fire) {
          regs(cout_index) := sum(64)
        }
      } .elsewhen (funct7(5, 4) === 2.U) { // sbcq
        sum := io.cmd.bits.rs1 -& io.cmd.bits.rs2 -& regs(cin_index)
      } .otherwise { // sbcqc
        sum := io.cmd.bits.rs1 -& io.cmd.bits.rs2 -& regs(cin_index)
        when (io.cmd.fire) {
          regs(cout_index) := sum(64)
        }
      }
    }
    io.resp.bits.data := sum(63, 0)
  } .elsewhen (opcode === "b0101011".U) { //custom1 conditional ops
    dont_resp := false.B
    when (funct7(6) === 0.U) { // addq
      when (funct7(5, 4) === 0.U) {
        // cin, non inverted
        val cond = regs(cout_index)
        sum := Mux(cond, io.cmd.bits.rs1 +& io.cmd.bits.rs2 +& regs(cin_index), io.cmd.bits.rs1)
      } .elsewhen (funct7(5, 4) === 1.U) { 
        // cin, inverted
        val cond = regs(cout_index)
        sum := Mux(~cond, io.cmd.bits.rs1 +& io.cmd.bits.rs2 +& regs(cin_index), io.cmd.bits.rs1)
      } .elsewhen (funct7(5, 4) === 2.U) {
        // cout, non inverted
        val cond = regs(cin_index)
        sum := Mux(cond, io.cmd.bits.rs1 +& io.cmd.bits.rs2, io.cmd.bits.rs1)
        when (io.cmd.fire) {
          regs(cout_index) := sum(64)
        }
      } .otherwise { // adcqc
        // cout, inverted
        val cond = regs(cin_index)
        sum := Mux(~cond, io.cmd.bits.rs1 +& io.cmd.bits.rs2, io.cmd.bits.rs1)
        when (io.cmd.fire) {
          regs(cout_index) := sum(64)
        }
      }
    } .otherwise {
      when (funct7(5, 4) === 0.U) {
        // cin, non inverted
        val cond = regs(cout_index)
        sum := Mux(cond, io.cmd.bits.rs1 -& io.cmd.bits.rs2 -& regs(cin_index), io.cmd.bits.rs1)
      } .elsewhen (funct7(5, 4) === 1.U) { 
        // cin, inverted
        val cond = regs(cout_index)
        sum := Mux(~cond, io.cmd.bits.rs1 -& io.cmd.bits.rs2 -& regs(cin_index), io.cmd.bits.rs1)
      } .elsewhen (funct7(5, 4) === 2.U) {
        // cout, non inverted
        val cond = regs(cin_index)
        sum := Mux(cond, io.cmd.bits.rs1 -& io.cmd.bits.rs2, io.cmd.bits.rs1)
        when (io.cmd.fire && cond) {
          regs(cout_index) := sum(64)
        }
      } .otherwise { // adcqc
        // cout, inverted
        val cond = regs(cin_index)
        sum := Mux(~cond, io.cmd.bits.rs1 -& io.cmd.bits.rs2, io.cmd.bits.rs1)
        when (io.cmd.fire && ~cond) {
          regs(cout_index) := sum(64)
        }
      }
    }
    io.resp.bits.data := sum(63, 0)
  } .elsewhen (opcode === "b1011011".U) { //custom2
    dont_resp := false.B
    val code = funct7(6, 4)
    when (code === 0.U) { // move
        dont_resp := true.B
        when (io.cmd.fire) {
          regs(cout_index) := regs(cin_index)
        }
    } .elsewhen (code === 1.U) { //mulhi
        val mul_res = Wire(UInt(128.W))

        mul_res := io.cmd.bits.rs1 * io.cmd.bits.rs2
        io.resp.bits.data := mul_res(127, 64)
    } .elsewhen (code === 2.U) { // mullo
        val mul_res = Wire(UInt(128.W))

        mul_res := io.cmd.bits.rs1 * io.cmd.bits.rs2
        io.resp.bits.data := mul_res(63, 0)
    } .elsewhen (code === 3.U) { // cdecq
        dont_resp := true.B
        when (io.cmd.fire) {
          regs(cout_index) := regs(cin_index) - regs(cout_index)
        }
    } .elsewhen (code === 4.U) { // shrqdq 1. MBITS=124
      io.resp.bits.data := Cat(io.cmd.bits.rs1, io.cmd.bits.rs2) >> (124-2-64)
    } .elsewhen (code === 5.U) { // shrqdq 2. 
      io.resp.bits.data := Cat(io.cmd.bits.rs1, io.cmd.bits.rs2) >> (1)
    } .elsewhen (code === 6.U) { // shrq
      io.resp.bits.data := io.cmd.bits.rs1 >> io.cmd.bits.rs2(5, 0)
    }
  } .elsewhen (opcode === "b1111011".U) { //custom3
    dont_resp := false.B
    val code = funct7(6, 4)
    when (code === 0.U) {
      // load from carry reg
      io.resp.bits.data := regs(cout_index)
    } .elsewhen (code === 1.U) {
      // store to carry reg
      dont_resp := true.B
      when (io.cmd.fire) {
        regs(cout_index) := io.resp.bits.data(0)
      }
    } .elsewhen (code === 2.U) {
      // czero
      when (regs(cout_index) === 1.U) {
        io.resp.bits.data := 0.U
      } .otherwise {
        io.resp.bits.data := io.cmd.bits.rs1
      }
    } .elsewhen (code === 3.U) {
      // cncmplq
      val cond = regs(cout_index)
      dont_resp := true.B
      when (io.cmd.fire && cond === 0.U) {
        regs(cin_index) := io.cmd.bits.rs1 < io.cmd.bits.rs2
      }
    } .elsewhen (code === 4.U) {
      // cncmpeq
      val cond = regs(cout_index)
      dont_resp := true.B
      when (io.cmd.fire && cond === 0.U) {
        regs(cin_index) := io.cmd.bits.rs1 === io.cmd.bits.rs2
      }
    } .elsewhen (code === 5.U) {
      // cncmpgq
      val cond = regs(cout_index)
      dont_resp := true.B
      when (io.cmd.fire && cond === 0.U) {
        regs(cin_index) := io.cmd.bits.rs1 > io.cmd.bits.rs2
      }
    } .elsewhen (code === 6.U) {
      // cncmpneq
      val cond = regs(cout_index)
      dont_resp := true.B
      when (io.cmd.fire && cond === 0.U) {
        regs(cin_index) := io.cmd.bits.rs1 =/= io.cmd.bits.rs2
      }
    } .otherwise {
      // andb
      dont_resp := true.B
      when (io.cmd.fire) {
        regs(cout_index) := regs(cin_index) & regs(cout_index)
      }
    }
  }

  io.cmd.ready := io.resp.ready || dont_resp
  io.busy := io.cmd.valid
  // boom doesn't commit properly
  io.resp.bits.rd := io.cmd.bits.inst.rd
  io.resp.valid := io.cmd.valid && (~dont_resp)
  io.interrupt := false.B
}
