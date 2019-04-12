package fpgabits

import scala.language.reflectiveCalls
import scala.math.pow
import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import scala.collection.mutable._

import core._
import commons.Const._

import testUtils._

class BRAMTester(m: BRAM) extends PeekPokeTester(m) {
  val c = m.c
  // need function to:
  //   1) set random address/data
  //   2) set sequential address/data
  //   3) needs option to enable write
  //   4) needs option to disable write (for read)
  def getPort(portNbr: Int): BRAMPort = m.io.getPort(portNbr)

  def enablePort(portNbr: Int): Unit =
    poke(getPort(portNbr).en, true)

  def disablePort(portNbr: Int): Unit =
    poke(getPort(portNbr).en, false)

  def setAddr(portNbr: Int, addr: BigInt): Unit =
    poke(getPort(portNbr).addr, addr)

  def getReadPort(portNbr: Int): Int = {
    if (c.isTrueDualPort){ portNbr }
    else { c.sdpReadPortIdx } }

  def getWritePort(portNbr: Int): Int = {
    if (c.isTrueDualPort){ portNbr }
    else { c.sdpWritePortIdx } }

  def limitWidth(portNbr: Int, data: BigInt): BigInt = {
    TestTools.limitWidth(data, c.dataWidthVec(portNbr))
  }

  def disableWrite(portNbr: Int): Unit = {
    val port = getWritePort(portNbr)
    poke(getPort(port).writeEn.get, false)}

  def enableWrite(portNbr: Int): Unit = {
    val port = getWritePort(portNbr)
    poke(getPort(port).writeEn.get, true)}

  def writeData(portNbr: Int, data: BigInt) : Unit = {
    val port = getWritePort(portNbr)
    poke(getPort(port).dataIn.get, limitWidth(port, data))}

  def readData(portNbr: Int) : BigInt = {
    val port = getReadPort(portNbr)
    peek(getPort(port).dataOut.get)}

  def readAtAddr(portNbr: Int, addr: BigInt) : BigInt = {
    val port = getReadPort(portNbr)
    setAddr(port, addr)
    enablePort(port)
    step(1)
    disablePort(port)
    readData(port) }

  def writeAtAddr(portNbr: Int, addr: BigInt, data: BigInt): Unit = {
    val port = getWritePort(portNbr)
    setAddr(port, addr)
    enablePort(port)
    enableWrite(port)
    writeData(port, data)
    step(1)
    disableWrite(port)
  }

  def getWritePortWidth(portNbr: Int): Int = {
    c.dataWidthVec(getWritePort(portNbr)) }
  def getReadPortWidth(portNbr: Int): Int = {
    c.dataWidthVec(getReadPort(portNbr)) }

  def seqWrite(portNbr: Int, dataQ: Queue[BigInt], offset: Int = 10){
    val port = getWritePort(portNbr)
    val localDataQ = dataQ.clone()
    for ( addr <- 0 until c.sizeVec(port) ) {
      val writeAddr = addr + offset
      val data = localDataQ.dequeue()
      writeAtAddr(portNbr, writeAddr, data) } }

  def randomWrite(portNbr: Int, addrQ: Queue[BigInt], dataQ: Queue[BigInt]){
    val localAddrQ = addrQ.clone()
    val localDataQ = dataQ.clone()
    while(!localAddrQ.isEmpty){
      writeAtAddr(portNbr, localAddrQ.dequeue, localDataQ.dequeue) } }

  def seqRead(portNbr: Int, offset: Int = 10): Queue[BigInt] = {
    // Make sure write is not enabled
    val port = getReadPort(portNbr)
    val readQ = new Queue[BigInt]
    for ( addr <- 0 until c.sizeVec(port) ) {
      val readAddr = addr + offset
      readQ.enqueue(readAtAddr(port, readAddr)) }
    readQ }

  def randomRead(portNbr: Int, addrQ: Queue[BigInt]) : Queue[BigInt] = {
    val localAddrQ = addrQ.clone()
    val readQ = new Queue[BigInt]
    while(!localAddrQ.isEmpty){
      readQ.enqueue(readAtAddr(portNbr, addrQ.dequeue)) }
    readQ
  }

  def mergeReadToSizeOfWrite(portNbr: Int, readQ: Queue[BigInt]): Queue[BigInt] = {
    val writePortWidth = getWritePortWidth(portNbr)
    val readPortWidth  = getReadPortWidth(portNbr)
    val widthRatio = writePortWidth / readPortWidth
    val binaryReadQ = readQ.map(_.toString(2).reverse.padTo(readPortWidth,  '0').reverse)
    val mergedReadQ = Queue[BigInt]()
    var baseStr = ""
    for(i <- 0 until c.sizeVec(c.sdpWritePortIdx))  {
      baseStr = ""
      for(j <- 0 until widthRatio){
        val dataRead = binaryReadQ(i*widthRatio + j)
        baseStr = dataRead + baseStr
      }
      mergedReadQ.enqueue(BigInt(baseStr, 2))
    }
    mergedReadQ
  }
}

class BRAMShouldReadSameAfterWrite(m: BRAM) extends BRAMTester(m){
  for(portNbr <- 0 until 1){
    val addr = 0
    val writePort = getWritePort(portNbr)
    val readPort  = getReadPort(portNbr)
    enablePort(writePort)
    enablePort(readPort)
    for (i <- 0 to 4){
      writeAtAddr(writePort, addr, i+5)
      assert(readAtAddr(readPort, addr) == i+5, "read is not the same as write") } }
}

class BRAMShouldWriteAndReadSequential(m: BRAM) extends BRAMTester(m){
  for (portNbr <- 0 until 1){
    val dataQ = TestTools.genRandomQueue(c.sizeVec(portNbr), c.dataWidthVec(portNbr))
    seqWrite(portNbr, dataQ, 0)
    val readQ = seqRead(portNbr, 0)

    val mergedReadQ = mergeReadToSizeOfWrite(portNbr, readQ)
    assert(mergedReadQ == dataQ, "ReadQ is not the same as dataQ")
  } }

class BRAMShouldWriteAndReadRandomAccess(m: BRAM) extends BRAMTester(m){
  for (portNbr <- 0 until 1){
    val dataQ = TestTools.genRandomQueue(c.sizeVec(portNbr), c.dataWidthVec(portNbr))
    val addrQ = TestTools.genUniqueRandomQueue(c.sizeVec(portNbr), c.addrWidthVec(portNbr))
    seqWrite(portNbr, dataQ, 0)
    val readQ = seqRead(portNbr, 0)

    val mergedReadQ = mergeReadToSizeOfWrite(portNbr, readQ)
    assert(mergedReadQ == dataQ, "ReadQ is not the same as dataQ")
  } }


class BRAMUnitTest extends FPGABitsFlatSpec {
  override val verbose = false

  override val moduleName = "BRAM"

  val behaviourList = List[((BRAM) => BRAMTester, String)](
    ((m) => new BRAMShouldReadSameAfterWrite(m),
     "read same after write at same address"),
    ((m) => new BRAMShouldWriteAndReadSequential(m),
     "write full depth sequentially"),
    ((m) => new BRAMShouldWriteAndReadRandomAccess(m),
     "write full depth with random access")
  )

  val confList = List[BRAMConfig](
    new BRAMConfig(false, Seq(4, 4), Seq(8,  8), Seq(8, 8)),        // SDP symmetric
    new BRAMConfig(Seq(constBRAM.TDPBRAM18ParamDict(9),
                       constBRAM.TDPBRAM18ParamDict(9))),           // TDP symmetric
    constBRAM.BRAM18SD32A13W2,                                      // SDP assymetric
    new BRAMConfig(Seq(constBRAM.TDPBRAM18ParamDict(18),            // TDP assymetric
                       constBRAM.TDPBRAM18ParamDict(9)))
  )

  val testEnable = List[Boolean](true, true, true)
  val confEnable = List[Boolean](true, true, true, true)

  assert(confEnable.length == confList.length, "confList must have same lenght as confEnable")
  confList.zip(confEnable).zipWithIndex.foreach{
    tuple => {
      implicit val conf = tuple._1._1
      val confEnable    = tuple._1._2
      val confIdx       = tuple._2
      val localTestEnable  = testEnable map {(x) => x && confEnable}

      execBehaviourList(behaviourList, localTestEnable,
                        () => new BRAM, confIdx)
    }
  }
}
