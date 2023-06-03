package spinal.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.com.i2c.I2c

case class I2cCommand() extends Bundle {
  val address = UInt(7 bits)
  val start = Bool()
  val read = Bool()
  val write = Bool()
  val writeMultiple = Bool()
  val stop = Bool()
}

case class I2cMasterStatus() extends Bundle {
  val busy = Bool()
  val busyControl = Bool()
  val busyActive = Bool()
  val missedAck = Bool()
}

case class I2cMasterConfig() extends Bundle {
  val preScale = UInt(16 bits)
  val stopOnIdle = Bool()
}

class I2cMaster() extends BlackBox {
  val io = new Bundle {
    val clk = in Bool ()
    val rst = in Bool ()
    val cmd = slave Stream (I2cCommand())
    val outData = slave Stream (Fragment(UInt(8 bits)))
    val inData = master Stream (Fragment(UInt(8 bits)))
    val status = out(I2cMasterStatus())
    val config = in(I2cMasterConfig())

    val i2c = master(I2c())
  }
  setDefinitionName("i2c_master")
  addRTLPath("./rtl/i2c_master.v")

  mapCurrentClockDomain(io.clk, io.rst)
  noIoPrefix()

  private def renameIO(): Unit = {
    io.flatten.foreach(bt => {
      var name = bt.getName()

      val prefixMap = Map(
        "cmd_" -> "s_axis_cmd_",
        "outData_" -> "s_axis_data_",
        "inData_" -> "m_axis_data_",
        "status_" -> "",
        "config_" -> ""
      )
      name = replacePrefixWithMap(name, prefixMap)

      if (name.contains("payload_")) name = name.replace("payload_", "")

      val suffixMap = Map(
        "data_fragment" -> "data_tdata",
        "data_valid" -> "data_tvalid",
        "data_ready" -> "data_tready",
        "data_last" -> "data_tlast",
        "_read" -> "_i",
        "_write" -> "_o",
      )
      name = replaceSuffixWithMap(name, suffixMap)

      name = camelToSnake(name)
      bt.setName(name)
    })
  }

  addPrePopTask(() => renameIO())
}

object I2cMasterVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Component {
      val i2c = new I2cMaster()
      i2c.io.cmd.setIdle()
      i2c.io.outData.setIdle()
      i2c.io.inData.setBlocked()
      i2c.io.config.assignDontCare()
      i2c.io.i2c.scl.read.assignDontCare()
      i2c.io.i2c.sda.read.assignDontCare()
    }.setDefinitionName("I2cMaster"))
  }
}
