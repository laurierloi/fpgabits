package fpgabits.testInterfaces

import scala.language.reflectiveCalls
import chisel3._
import chisel3.iotesters._
import scala.collection.mutable.Queue
import scala.collection.mutable

import fpgabits.testUtils.TestTools._
import fpgabits.interfaces._

trait AxiStreamTestInterface extends PeekPokeTests {

  case class AxiStreamScala(val data  : BigInt, val valid : Boolean,
                            val last  : Boolean, val ready : Boolean){
    def this(axis: AxiStreamScala, ready: Boolean){
      this(axis.data, axis.valid, axis.last, ready) }
    def this(axis: AxiStreamScala, data: BigInt, valid: Boolean, last: Boolean){
      this(data, valid, last, axis.ready) }
    override def toString: String = f"\n(data: $data%5d, valid :$valid%5s, last: $last%5s, ready: $ready%5s)"
    def fire          : Boolean =  ready &&  valid
    def readyNotValid : Boolean =  ready && !valid
    def validNotReady : Boolean = !ready &&  valid
  }

  val axiStreamList : List[AxiStreamSignals[Bits]]
  val maxIte = 100

  def int(x: Int):  BigInt = BigInt(x)
  def int(x: Long): BigInt = BigInt(x)

  var axiIdx: Int = 0

  var dataWidth = 0


  // Set function
  def setAxiStreamReady(ready : Boolean, idx : Int) : Unit =
    poke(axiStreamList(idx).tready,    ready)
  def setAxiStreamValid(valid : Boolean, idx : Int) : Unit =
    poke(axiStreamList(idx).tvalid,    valid)
  def setAxiStreamLast(last   : Boolean, idx : Int) : Unit =
    poke(axiStreamList(idx).tlast.get, last)
  def setAxiStreamData(data   : BigInt,  idx : Int) : Unit =
    poke(axiStreamList(idx).tdata,     data)

  def setAxiStreamLastIfExist(last : Boolean,  idx : Int = axiIdx) : Unit =
    if(axiStreamList(idx).withTLast) { setAxiStreamLast(last, idx) }

  // Get Function
  def getAxiStreamReady(idx : Int) : Boolean = peek(axiStreamList(idx).tready)        != 0
  def getAxiStreamValid(idx : Int) : Boolean = peek(axiStreamList(idx).tvalid)        != 0
  def getAxiStreamLast(idx  : Int) : Boolean = peek(axiStreamList(idx).tlast.get)     != 0
  def getAxiStreamData(idx  : Int) : BigInt  = peek(axiStreamList(idx).tdata)
  def getAxiStreamFire(idx  : Int) : Boolean =
    getAxiStreamValid(idx)  & getAxiStreamReady(idx)
  def getAxiStreamValidNotReady(idx : Int) : Boolean =
    getAxiStreamValid(idx)  & !getAxiStreamReady(idx)
  def getAxiStreamReadyNotValid(idx : Int) : Boolean =
    !getAxiStreamValid(idx) & getAxiStreamReady(idx)

  def getAxiStreamLastIfExist(idx : Int)   : Boolean =
    if(axiStreamList(idx).withTLast) getAxiStreamLast(idx) else false

  def writeAxiStreamDataNoStep(data: BigInt, valid: Boolean, last: Boolean = false, idx: Int) = {
    setAxiStreamData(data, idx)
    setAxiStreamValid(valid, idx)
    setAxiStreamLastIfExist(last, idx)}

  def readAxiStreamDataNoStep(idx : Int): (BigInt, Boolean, Boolean, Boolean) =
    (getAxiStreamData(idx), getAxiStreamValid(idx), getAxiStreamLastIfExist(idx), getAxiStreamReady(idx))

  def writeAxiStreamScalaDataNoStep(axis: AxiStreamScala, idx: Int): Unit =
    writeAxiStreamDataNoStep(axis.data, axis.valid, axis.last, idx)

  def writeAxiStreamScalaReadyNoStep(axis: AxiStreamScala, idx: Int): Unit =
    setAxiStreamReady(axis.ready, idx)

  def readAxiStreamScalaDataNoStep(idx: Int) : AxiStreamScala = {
    val readData = readAxiStreamDataNoStep(idx)
    AxiStreamScala(readData._1, readData._2, readData._3, readData._4) }

  def readAxiStreamScalaReadyNoStep(currentAxi: AxiStreamScala, idx: Int) : AxiStreamScala = {
    val ready = getAxiStreamReady(idx)
    new AxiStreamScala(currentAxi, ready)
  }

  def writeAxiStreamScalaDataUntilFire(axis: AxiStreamScala, idx: Int): Unit = {
    // NOTE: 1 cycle of delay is added between the !validNotReady and the actual input change
    //       this is required to keep the process synchronous
    writeAxiStreamScalaDataNoStep(axis, idx)
    var ctr = 0
    var validNotReady = true
    while(validNotReady && ctr < maxIte){
      validNotReady = getAxiStreamValidNotReady(idx)
      step(1)
      ctr += 1
    }
    assert(ctr != maxIte, "Reached Max Ite")
    setAxiStreamValid(false, idx)
  }

  def readAxiStreamScalaDataUntilFire(idx: Int) : AxiStreamScala = {
    var ctr = 0
    var data = new AxiStreamScala(0, false, false, false)
    do{
      data = readAxiStreamScalaDataNoStep(idx)
      step(1)
      ctr += 1
    } while(data.readyNotValid && ctr < maxIte)
    assert(ctr != maxIte, "Reached Max Ite")
    //if(ctr == maxIte) {println("Reached Max Ite!")}
    setAxiStreamReady(false, idx)
    data
    }

  def genAxiStreamScalaDataQueue(dataQ: Queue[BigInt], validQ: Queue[Boolean],
                                 lastQ: Queue[Boolean] = Queue()): Queue[AxiStreamScala] = {
    val localDataQ  = dataQ.clone()
    val localValidQ = validQ.clone()
    val localLastQ  = lastQ.clone()
    val streamQ     = Queue[AxiStreamScala]()
    while(localDataQ.nonEmpty){
      val data  = localDataQ.dequeue()
      val valid = localValidQ.dequeue()
      val last  = if(localLastQ.nonEmpty) localLastQ.dequeue() else false
      streamQ.enqueue(new AxiStreamScala(data, valid, last, false))
    }
    streamQ
  }

  // TODO: maybe use some form of a parameter map?
  def getAxiStreamScalaDataQueue(nbrData: Int, width: Int, validBias: Double = 1.0,
                                 signed: Boolean = false, lastIdx: Seq[Int] = Seq(),
                                 customGen: String = "", nbrItemPerData: Int = 1): Queue[AxiStreamScala] = {
    val gen = if(customGen != "") customGen else if(signed) "SignedRandom" else "Random"
    val validDataQ   = genValidConcatDataQueueWithNbrValid(nbrItemPerData, nbrData, gen, width, validBias)
    val validDataIdx = validDataQ.map(_._1).zipWithIndex.collect { case (true, idx) => idx}
    val lastQIdx     = validDataIdx.zipWithIndex.filter{ case (x, idx) => lastIdx.contains(idx)}.map(_._1)
    val lastQ        = validDataQ.zipWithIndex.map     { case (x, idx) => lastQIdx.contains(idx)}
    genAxiStreamScalaDataQueue(validDataQ.map(_._2), validDataQ.map(_._1), lastQ)}

  def genAxiStreamScalaReadyQueue(readyQ: Queue[Boolean]) : Queue[AxiStreamScala] = {
    val localReadyQ = readyQ.clone()
    val streamQ     = Queue[AxiStreamScala]()
    while(localReadyQ.nonEmpty){
      streamQ.enqueue(new AxiStreamScala(0, false, false, readyQ.dequeue())) }
    streamQ }

  def writeQueueIntoAxi(axisQ: Queue[AxiStreamScala], idx: Int){
    val localAxisQ = axisQ.clone()
    while(localAxisQ.nonEmpty){
      writeAxiStreamScalaDataUntilFire(localAxisQ.dequeue, idx)
    }
  }

  def readQueueFromAxiWithReady(readyQ: Queue[Boolean], idx: Int) : Queue[AxiStreamScala] = {
    val localReadyQ = readyQ.clone()
    val streamQ = Queue[AxiStreamScala]()
    while(localReadyQ.nonEmpty){
      val ready = localReadyQ.dequeue()
      setAxiStreamReady(ready, idx)
      streamQ.enqueue(readAxiStreamScalaDataUntilFire(idx)) }
    streamQ
  }

  def writeMultipleAxiDataNoStep(axisDatas: Seq[AxiStreamScala], idxList: Seq[Int]): Unit = {
    idxList.zip(axisDatas).foreach{
      case (idx, axis) => writeAxiStreamScalaDataNoStep(axis, idx) }}

  def writeMultipleAxiReadyNoStep(readyS: Seq[Boolean], idxList: Seq[Int]): Unit = {
    idxList.zip(readyS).foreach{
      case (idx, ready) => setAxiStreamReady(ready, idx) }}

  def readMultipleAxiDataNoStep(idxList: Seq[Int]): Seq[AxiStreamScala] = {
    idxList.map{ case i => readAxiStreamScalaDataNoStep(i)}}

  def readWriteMultipleAxiNoStep(writeData: Seq[AxiStreamScala], writeIdxList: Seq[Int],
                                 ready:     Seq[Boolean], readIdxList: Seq[Int]): Seq[AxiStreamScala] = {
    writeMultipleAxiDataNoStep(writeData, writeIdxList)
    readMultipleAxiDataNoStep(readIdxList) }

  def writeMultipleAxi(writeData: Seq[Queue[AxiStreamScala]], writeIdxList: Seq[Int]): Unit = {

    writeIdxList.foreach {case (idx) => setAxiStreamValid(false, idx)}

    val localWriteData    = writeData.map(x => x.clone())
    var changeWriteNext   = Seq.fill (writeIdxList.length) (false)
    var writeLoopCounters = mutable.Seq.fill (writeIdxList.length) (0)

    while(!localWriteData.map(_.isEmpty).fold(true)(_ && _)){

      // Data must be changed 1 cycle after match to ensure synchronous behaviour
      changeWriteNext.zip(localWriteData).zipWithIndex.foreach{
        case ((true, dataQ), idx) => {
          writeLoopCounters(idx) = 0
          if(dataQ.nonEmpty) { writeAxiStreamScalaDataNoStep(dataQ.dequeue(), writeIdxList(idx)) }
          else               { setAxiStreamValid(false, writeIdxList(idx)) }}
        case (_) => // do Nothing
      }


      changeWriteNext = writeIdxList.map(!getAxiStreamValidNotReady(_))
      step(1)
      // Must be carefull with loop counters so they only count up when we are waiting for a response
      // from the module
      writeLoopCounters = writeLoopCounters.zipWithIndex.map{
        case(ctr, idx) => if(getAxiStreamValidNotReady(idx)) ctr + 1
                          else ctr }

      // Many loops, which we hope are not infinite but it sometime happens...
      val writeCounterBelowMax = writeLoopCounters.map(_ < maxIte).fold(false)(_ || _)
      assert(writeCounterBelowMax, s"A write Counter busted: max: ${maxIte}, counters: ${writeLoopCounters}")
    }

  }

  def readWriteMultipleAxi(writeData: Seq[Queue[AxiStreamScala]], writeIdxList: Seq[Int],
                           readReady: Seq[Queue[Boolean]], readIdxList: Seq[Int]):
      Seq[Queue[AxiStreamScala]] = {
    // The two loops below make sure that the input are set before we start
    writeIdxList.foreach {case (idx) => setAxiStreamValid(false, idx)}
    readIdxList.foreach  {case (idx) => setAxiStreamReady(false, idx)}

    // Need to use local queues to avoid effects on inputs
    val localWriteData    = writeData.map(x => x.clone())
    val localReadReady    = readReady.map(x => x.clone())

    val readData          = Seq.fill(readIdxList.length)(Queue[AxiStreamScala]())
    val targetReadData    = readReady.map(_.filter(_ == true).length)
    val readDataDone      = mutable.Seq.fill(readIdxList.length)(false)

    var changeWriteNext   = Seq.fill (writeIdxList.length) (false)
    var writeLoopCounters = mutable.Seq.fill (writeIdxList.length) (0)
    var readyLoopCounters = mutable.Seq.fill (readIdxList.length)  (0)

    // End condition is when each read Queues are empty and the last data has fired
    while(!readDataDone.fold(true)(_ && _)){

      // Data must be changed 1 cycle after match to ensure synchronous behaviour
      changeWriteNext.zip(localWriteData).zipWithIndex.foreach{
        case ((true, dataQ), idx) => {
          writeLoopCounters(idx) = 0
          if(dataQ.nonEmpty) { writeAxiStreamScalaDataNoStep(dataQ.dequeue(), writeIdxList(idx)) }
          else               { setAxiStreamValid(false, writeIdxList(idx)) }}
        case (_) => // do Nothing
      }

      // readData must be before read Ready so that we can get the measurement for "fire"
      val readFire = readIdxList.map(getAxiStreamFire(_))
      readFire.zip(readData).zip(readIdxList).foreach{
        case ((true, readQ), idx) => readQ.enqueue(readAxiStreamScalaDataNoStep(idx))
        case (_) => // do Nothing
      }

      // Ready is consumed on the same cycle
      // on the next cycle, ready would have been true for too long
      (readIdxList.map(!getAxiStreamReadyNotValid(_))).zip(localReadReady).zipWithIndex.foreach{
        case ((true, readyQ), idx) => {
          readyLoopCounters(idx) = 0
          if(readyQ.nonEmpty) {
            setAxiStreamReady(readyQ.dequeue(), readIdxList(idx)) }
          else{
            readDataDone(idx) = true
            setAxiStreamReady(false, readIdxList(idx))} }
        case (_) => // do Nothing
      }

      changeWriteNext = writeIdxList.map(!getAxiStreamValidNotReady(_))
      step(1)
      // Must be carefull with loop counters so they only count up when we are waiting for a response
      // from the module
      writeLoopCounters = writeLoopCounters.zipWithIndex.map{
        case(ctr, idx) => if(getAxiStreamValidNotReady(idx)) ctr + 1
                          else ctr }
      readyLoopCounters = readyLoopCounters.zipWithIndex.map{
        case(ctr, idx) => if(getAxiStreamReadyNotValid(idx)) ctr + 1
                          else ctr }

      // Many loops, which we hope are not infinite but it sometime happens...
      val writeCounterBelowMax = writeLoopCounters.map(_ < maxIte).fold(false)(_ || _)
      val readyCounterBelowMax = readyLoopCounters.map(_ < maxIte).fold(false)(_ || _)
      assert(writeCounterBelowMax, s"A write Counter busted: max: ${maxIte}, counters: ${writeLoopCounters}")
      assert(readyCounterBelowMax, s"A ready Counter busted: max: ${maxIte}, counters: ${readyLoopCounters}")
    }
    readData
  }

  def axiStreamScalaQueueKeepOnlyValid(axiQ: Queue[AxiStreamScala])     : Queue[AxiStreamScala] = {
    axiQ.filter(_.valid == true) }
  def axiStreamScalaQueueKeepOnlyFire(axiQ: Queue[AxiStreamScala])      : Queue[AxiStreamScala] = {
    axiQ.filter(axi => axi.valid == true & axi.ready == true) }
  def axiStreamScalaQueueKeepOnlyValidData(axiQ: Queue[AxiStreamScala]) : Queue[BigInt] = {
    axiStreamScalaQueueKeepOnlyValid(axiQ).map(_.data) }
  def axiStreamScalaQueueKeepOnlyFireData(axiQ: Queue[AxiStreamScala])  : Queue[BigInt] = {
    axiStreamScalaQueueKeepOnlyFire(axiQ).map(_.data) }

}
