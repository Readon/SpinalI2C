package spinal.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.com.i2c.I2c

case class I2cSlaveStatus() extends Bundle {
  val busy = Bool()
  val busAddress = UInt(7 bits)
  val busAddressed = Bool()
  val busActive = Bool()
}

case class I2cSlaveConfig() extends Bundle {
  val enable = Bool()
  val deviceAddress = UInt(7 bits)
  val deviceAddressMask = UInt(7 bits)
}

class I2cSlave(filterLen: Int = 4) extends BlackBox {
  val io = new Bundle {
    val clk = in Bool ()
    val rst = in Bool ()
    val releaseBus = in Bool ()
    val outData = slave Stream (Fragment(UInt(8 bits)))
    val inData = master Stream (Fragment(UInt(8 bits)))
    val status = out(I2cSlaveStatus())
    val config = in(I2cSlaveConfig())

    val i2c = master(I2c())
  }

  setDefinitionName("i2c_slave")
  addRTLPath("./rtl/i2c_slave.v")
  addGeneric("FILTER_LEN", filterLen)

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
        "config_" -> "",
        "i2c_" -> ""
      )
      name = replacePrefixWithMap(name, prefixMap)

      if (name.contains("payload_")) name = name.replace("payload_", "")

      val suffixMap = Map(
        "data_fragment" -> "data_tdata",
        "data_valid" -> "data_tvalid",
        "data_ready" -> "data_tready",
        "data_last" -> "data_tlast",
        "_read" -> "_i",
        "_write" -> "_o"
      )
      name = replaceSuffixWithMap(name, suffixMap)

      name = camelToSnake(name)
      bt.setName(name)
    })
  }

  addPrePopTask(() => renameIO())
}

object I2cSlaveVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Component {
      val i2c = new I2cSlave()
      i2c.io.outData.setIdle()
      i2c.io.inData.setBlocked()
      i2c.io.config.assignDontCare()
      i2c.io.releaseBus.assignDontCare()
      i2c.io.i2c.scl.read.assignDontCare()
      i2c.io.i2c.sda.read.assignDontCare()
    }.setDefinitionName("I2cSlave"))
  }
}
