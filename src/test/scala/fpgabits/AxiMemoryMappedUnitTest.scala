package fpgabits

import scala.language.reflectiveCalls
import scala.math.pow

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import scala.collection.mutable._

import interfaces._

import commons.Const._

import testInterfaces._
import testUtils.TestTools._
import testUtils._

/** Test Objectives:
  * 1) Validate addressWrite and Write handshake with writeResponse
  * 2) Validate addressRead with readResponse
  * 3) Test register write functionalities:
  *   1. write register
  *   2. write register with strobe
  *   3. write register which is read only
  *   4. write pulsed register
  * 4) Test register read functionalities:
  *   1. read register
  *   2. read pulsed register (in combination with 3.4)
  * 5) Test conflicts:
  *   1. read and write on same register at the same time
  *   2. read and write on different registers
  * 6) Test response timings:
  *   1. make sure that read/write response comes with valid response
  */
class AxiMemoryMappedTester(m: AxiMemoryMappedWithRegs)
  extends PeekPokeTester(m) with HighLevelAxiLiteTestInterface{
  override val axiLiteList = List[AxiLiteSignals](m.io.axiLite)
  axiIdx = 0
  dataWidth = m.c.dataWidth

  def getRegValQueue() : Queue[BigInt] = {
    val q = Queue[BigInt]()
    for (i <- 0 until m.c.nbrReg)
      q.enqueue(getRegVal(i))
    q
  }
  // Register Interface
  def getRegVal(regNbr: Int): BigInt =
    peek(m.io.regsValues(regNbr))
  def expectRegVal(regNbr: Int, regValExpect: BigInt): Unit =
    expect(m.io.regsValues(regNbr), regValExpect)
  def setRegVal(regNbr: Int, value: BigInt): Unit =
    poke(m.io.moduleInputs(regNbr), value)

  def expectShiftedRegVal(regNbr: Int, data: BigInt, byteWidth: Int, byteShift: Int){
    val regVal = getRegVal(regNbr)
    val shiftedRegValMod = (regVal >> (byteShift*8)) % doubleToBigInt(pow(2, (byteWidth*8)))
    expect(shiftedRegValMod == data,
           s"Reg Value ${shiftedRegValMod} isn't the same as expected ${data}" +
           s" ByteShift: ${byteShift}, byteWidth: ${byteWidth}, regVal: ${regVal}")
  }

  // Reset Cycle
  def doResetCycle = {
    poke(m.io.resetn, false.B)
    step(2)
    poke(m.io.resetn, true.B)
  }
}


// 1) Validate addressWrite and Write handshake with writeResponse
class AxiMemoryMappedWriteCycleTest(m: AxiMemoryMappedWithRegs)
    extends AxiMemoryMappedTester(m){

  // 3 cases:
  //   1) Write address then Write Data
  //   2) Write data then write address
  //   3) Write data and address at the same time
  // Response is
  //   - rOKAY   if the address is not read only
  //   - rSLVERR if the address is read only

  val WriteAddrThenData = 0
  val WriteDataThenAddr = 1
  val WriteDataAndAddr  = 2
  val checkReg = true

  doResetCycle

  for ( caseSel <- 0 until 3) {
    //print(s"Doing case: $caseSel\n")
    val dataToWriteSeq = Queue[BigInt]()
    for ( addr <- 0 until m.c.nbrReg ){
      //print(s"Doing addr: $addr\n")
      val dataToWrite = BigInt(f"0FADED${addr}%02d", 16)
      dataToWriteSeq.enqueue(dataToWrite)
      //print("DataToWrite: " + f"${dataToWrite}\n")
      // Write data into the register
      caseSel match{
        case WriteAddrThenData => {
          setWriteAddress(addr*4)
          writeAddrSelectiveFire(true, false)
          setWriteData(dataToWrite)
          writeAddrSelectiveFire(false, true)
        }
        case WriteDataThenAddr => {
          setWriteData(dataToWrite)
          step(1)
          setWriteAddress(addr*4)
          writeAddrSelectiveFire(true, true)
        }
        case WriteDataAndAddr => {
          setWriteAddress(addr*4)
          setWriteData(dataToWrite)
          writeAddrSelectiveFire(true, true)
        }
      }
      // get the response and validate its value
      val addressIsReadOnly = m.c.readOnly(addr)
      val expectedWriteResponse = if(addressIsReadOnly) m.c.rSLVERR
                                  else                  m.c.rOKAY

      setWriteResponseReady(true)
      var ite = 0
      var gotWriteResponse = false
      //print("Waiting to get expected write response\n")
      while(!gotWriteResponse & ite < maxIte) {
        if (getWriteResponseFire){
          expectWriteResponse(expectedWriteResponse)
          gotWriteResponse = true
        }
        step(1)
        ite += 1
      }
    }

    // check register values
    if(checkReg){
      dataToWriteSeq.zipWithIndex.foreach{
        case(data, idx) => if (m.c.pulseMode(idx)){
                              expectRegVal(idx, 0)}
                           else if (!m.c.readOnly(idx)){
                              expectRegVal(idx, data)}
      }
    }
  }
}


class AxiMemoryMappedReadCycleTest(m: AxiMemoryMappedWithRegs)
      extends AxiMemoryMappedWriteCycleTest(m){
  doResetCycle
  // Write cycle is already done, now lets check if the value read are as expected
  val readDataAndResponseQueue = Queue[(BigInt, BigInt)]()
  for ( addr <- 0 until m.c.nbrReg ){
    val readDataAndResponse = setReadAddressAndGetData(addr*4)
    expect(m.io.regsValues(addr), readDataAndResponse._1)
    readDataAndResponseQueue.enqueue(readDataAndResponse)
  }
  val regValQueue = getRegValQueue
  //print(s"Read data and response: ${readDataAndResponseQueue}\n")
  //print(s"Output registers: ${regValQueue}\n")
}

class AxiMemoryMappedReadOnlyTest(m: AxiMemoryMappedWithRegs) extends AxiMemoryMappedTester(m){
  doResetCycle
  val readOnlyRegs      = m.c.readOnly
  val valsToWriteStr    = Queue[String]("00000000", "11111111", "133759AD", "02412412", "12345678")
  val valsToWrite       = valsToWriteStr map {BigInt(_, 16)}
  val readOnlyRegsIndex = readOnlyRegs.zipWithIndex.filter(_._1 == true) map (_._2)
  //print(s"valsToWrite: ${valsToWrite}\n")
  //print(s"readOnlyRegsIndex: ${readOnlyRegsIndex}\n")
  valsToWrite.foreach{
    case valToWrite => {
      // write value
      readOnlyRegsIndex.foreach{
        case reg => {
          setRegVal(reg, valToWrite+reg)
          step(1)
        }
      }
      // expect register value
      readOnlyRegsIndex.foreach{
        case reg => {
          expectRegVal(reg, valToWrite+reg)
          step(1)
        }
      }
      // expect read value
      readOnlyRegsIndex.foreach{
        case reg => {
          val readDataAndResponse = setReadAddressAndGetData(reg*4)
          expect(m.io.regsValues(reg), readDataAndResponse._1)
          step(1)
        }
      }
    }
  }
}

class ShouldWriteStrobedDataProperly(m: AxiMemoryMappedWithRegs) extends AxiMemoryMappedTester(m){
  doResetCycle
  def addressGen(offset: BigInt)(x: BigInt): BigInt = {
    BigInt(x.toString(2) + offset.toString(2), 2)
  }
  val maxOffset = 3
  val nbrData = m.c.nbrReg
  val data32Bit = genQueue(nbrData, "Random", 32)
  val data16Bit = genQueue(nbrData, "Random", 16)
  val data8Bit  = genQueue(nbrData, "Random",  8)
  val addressesOffsets = (0 to maxOffset) map {
    x => genQueue(nbrData, "Custom", m.c.addrWidth, 0.0,addressGen(x))
  }

  // 32Bit, no offset
  addressesOffsets.zipWithIndex.foreach{
    tuple => {
      val addresses = tuple._1
      addresses.zipWithIndex.foreach{
        addrTuple => {
          val addr = addrTuple._1
          val idx  = addrTuple._2
          val regAddr = addr.toInt / 4
          val offset = addr.toInt % 4
          if (! (m.c.readOnly(regAddr) || m.c.pulseMode(regAddr)) ){
            // 32 bit writes
            if (offset == 0){
              writeData32Bit(addr, data32Bit(idx))
              step(1)
              expectRegVal(regAddr, data32Bit(idx))
            }
            // 16 bit writes
            if (offset < 3){
              writeData16Bit(addr, data16Bit(idx))
              step(1)
              expectShiftedRegVal(regAddr, data16Bit(idx), 2, offset)
            }
            // 8 bit writes
            writeData8Bit(addr, data8Bit(idx))
            step(1)
            expectShiftedRegVal(regAddr, data8Bit(idx), 1, offset)
          }
        }
      }
    }
  }
}

class AxiMemoryMappedUnitTest extends FPGABitsFlatSpec {
  override val verbose = false

  override val moduleName = "AxiMemoryMapped"

  val nbrRegs      = 10
  val regWidth     = 32
  val readOnly     = List(0,1,0,0,0,1,0,0,0,1).map(_ == 1)
  val pulseMode    = List(0,0,0,1,0,0,0,0,1,0).map(_ == 1)
  val confList     = Seq[AxiMemoryMappedConfig](
    new AxiMemoryMappedConfig(nbrRegs, readOnly, pulseMode))


  val behaviourList = List[((AxiMemoryMappedWithRegs) => AxiMemoryMappedTester, String)](
    (m => new AxiMemoryMappedWriteCycleTest(m),
     "test the default AxiMemoryMapped write cycle and validate proper Reg values"),
    (m => new AxiMemoryMappedReadCycleTest(m),
     "test the default AxiMemoryMapped read cycle and validate proper Reg values"),
    (m => new AxiMemoryMappedReadOnlyTest(m),
     "test AxiMemoryMapped readOnly registers"),
    (m => new ShouldWriteStrobedDataProperly(m),
     "test strobe with all possible offsets")
  )

  /* Which Conf and tests */
  val testEnable = List[Boolean](true, true, true, true)
  val confEnable = List[Boolean](true)

  confList.zip(confEnable).zipWithIndex.foreach{
    tuple => {
      implicit val conf = tuple._1._1
      val confEnable    = tuple._1._2
      val confIdx       = tuple._2
      val localTestEnable  = testEnable map {(x) => x && confEnable}

      execBehaviourList(behaviourList, localTestEnable,
                        () => new AxiMemoryMappedWithRegs, confIdx)
    }
  }
}
