package spinal.i2c

import spinal.core._

class I2cCommand extends Bundle{
    val address = UInt(7 bits)
    val start = Bool()
    val read = Bool()
    val write = Bool()
    val writeMultiple = Bool()
    val stop = Bool()
}

class I2cMasterStatus extends Bundle{
    val busy = Bool()
    val busyControl = Bool()
    val busyActive = Bool()
    val missedAck = Bool()
}

class I2cMasterConfig extends Bundle{
    val preScalar = UInt(16 bits)
    val stopOnIdle = Bool()
}

class I2cMaster() extends BlackBox {
    val io = new Bundle {
        val cmd = slave Stream(I2cCommand)
        val outData = slave Stream(Fragment(UInt(8 bits)))
        val inData = master Stream(Fragment(UInt(8 bits)))
        val status = out I2cMasterStatus
        val config = in I2cMasterConfig
    }
}