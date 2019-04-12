package fpgabits.testUtils

import scala.math.pow
import scala.util.Random
import chisel3._
import scala.collection.mutable.Queue

object TestTools{

  def bitStringTwosComplement(x: String): String = {
    val reversed = x.map{ case '1' => '0'; case '0' => '1'}
    val added    = BigInt(reversed, 2) + 1
    added.toString(2)}

  def getPaddedHexString(x: BigInt, size: Int): String = {
    // NOTE: this doesn't manage sign extension
    val bitString = getPaddedBitString(x, size)
    BigInt(bitString, 2).toString(16).reverse.padTo(size/4, '0').reverse }

  def getPaddedBitString(x: BigInt, size: Int): String = {
    val isNegative = x < 0
    val actualSize = if(isNegative) size - 1 else size
    // TODO: this doesn't work if x > 2^size
    val baseBitString = x.toString(2).takeRight(actualSize).replace("-", "").reverse.padTo(size, '0').reverse
    val bitString = if (isNegative){ // Do Two's complement
      bitStringTwosComplement(baseBitString).takeRight(size).reverse.padTo(size, '1').reverse
    } else {
      baseBitString }
    //if (isNegative) println(s"${x}, ${bitString}")
    bitString}

  def concatData(dataSeq: Seq[BigInt], w: Int): BigInt = {
    val dataStrings = dataSeq.map(getPaddedBitString(_, w))
    val concatDataString = dataStrings.foldLeft(""){case (x, y) => y + x}
    //if (dataStrings.length > 1) println(s"ConcatData: $concatDataString")
    BigInt(concatDataString, 2) }

  def splitData(data: BigInt, w: Int, nbrData: Int, signed: Boolean = false): Seq[BigInt] = {
    val dataString = data.toString(2).reverse.padTo(w*nbrData, '0').reverse
    dataString.grouped(w).toSeq.reverse.map{
      case x => { if(signed && x.head == '1') { -BigInt(bitStringTwosComplement(x), 2)}
                  else {BigInt(x, 2) }}} }

  def splitDataSeq(dataSeq: Seq[BigInt], w: Int, nbrValPerData: Int, signed: Boolean = false): Seq[BigInt] = {
    dataSeq.map(splitData(_, w, nbrValPerData, signed)).flatten }

  def toggleSignal(f: Boolean => Unit, step: Int => Unit): Unit = {
    f(true)
    step(1)
    f(false) }


  def limitWidth(x: BigInt, w: Int, signed: Boolean = false): BigInt = {
    x % BigDecimal(pow(2, w)).toBigInt}

  def doubleToBigInt(x: Double): BigInt = BigDecimal(x).toBigInt

  def getSeededRandom(seed: Int): Random = new Random(seed)
  val testToolsSeed   = 0x13375EED
  val testToolsRandom = getSeededRandom(testToolsSeed)

  // TODO: maybe generator should be a Class/object pair with clean interface instead of this mess

  def genCustom        (x: BigInt, w: Int, gen: (BigInt) => BigInt): BigInt = limitWidth(gen(x), w)
  def genZero          (x: BigInt, w: Int): BigInt = limitWidth(0, w)
  def genLinear        (x: BigInt, w: Int): BigInt = limitWidth(x+1, w)
  def genLinearNeg     (x: BigInt, w: Int): BigInt = {limitWidth(-x-1, w, true)}
  def genPlusOneSquare (x: BigInt, w: Int): BigInt = limitWidth(doubleToBigInt(pow((x.toInt+1), 2)), w)
  def genRandomData       (w: Int): BigInt = BigInt(w.toInt, testToolsRandom)
  def genSignedRandomData (w: Int): BigInt = {
    val data = genRandomData(w-1)
    if(testToolsRandom.nextBoolean()) -data else data }
  def genRandomBoolBiased (trueBias: Double): BigInt = {
    assert(trueBias >= 0.0 && trueBias <= 1.0,
           s"trueBias Should be in the interval: [0.0, 1.0], actual: ${trueBias}")
    val randomVal = testToolsRandom.nextInt(10000)/10000.0
    if (randomVal <= trueBias) 1 else 0 }
  def genRandomBool: BigInt        = genRandomBoolBiased(0)

  final val generatorOptions = List(
    "Custom", "Zero", "Linear", "LinearNeg", "PlusOneSquare",
    "Random", "SignedRandom",
    "RandomBoolBiased", "RandomBool")
  def baseCustomGen(x: BigInt): BigInt = x

  def genQueue(nbrItem: Int, genName: String,
               w: Int, trueBias: Double = 0.0,
               customGen: (BigInt) => BigInt = baseCustomGen): Queue[BigInt] = {
    val stream = Queue[BigInt]()
    for (i <- 0 until nbrItem){
      val newData = genName match {
        case "Zero"             => genZero(i, w)
        case "Linear"           => genLinear(i, w)
        case "LinearNeg"        => genLinearNeg(i, w)
        case "PlusOneSquare"    => genPlusOneSquare(i, w)
        case "Random"           => genRandomData(w)
        case "SignedRandom"     => genSignedRandomData(w)
        case "RandomBoolBiased" => genRandomBoolBiased(trueBias)
        case "RandomBool"       => genRandomBool
        case _                  => genCustom(i, w, customGen)
      }
      stream.enqueue(newData)
    }
    stream
  }

  def genRandomQueue(nbrItem: Int, width: Int): Queue[BigInt] = {
    genQueue(nbrItem, "Random", width) }

  def genUniqueRandomQueue(nbrItem: Int, width: Int): Queue[BigInt] = {
    val notUniqueQ = Queue[BigInt]()
    while(notUniqueQ.distinct.length < nbrItem){
      notUniqueQ ++= genQueue(nbrItem, "Random", width)
    }
    notUniqueQ.distinct.slice(0, nbrItem)
  }

  def genZeroQueue(nbrItem: Int): Queue[BigInt] = {
    genQueue(nbrItem, "Zero", 8) }

  def genBiasedBoolQueue(nbrItem: Int, trueBias: Double): Queue[Boolean] = {
    val q = genQueue(nbrItem, "RandomBoolBiased", 1, trueBias).map(_ != 0)
    q
  }

  def genBiasedBoolQueueWithNbrTrue(nbrTrue: Int, trueBias: Double) : Queue[Boolean] = {
    val myQ = Queue[Boolean]()
    while(myQ.count(_ == true) < nbrTrue){
      myQ ++= genBiasedBoolQueue(nbrTrue, trueBias)
    }
    val validIdx       = myQ.zipWithIndex.collect{case (true, index) => index}
    val idxOfLastValid = validIdx(nbrTrue-1)
    myQ.slice(0, idxOfLastValid+1)
  }

  def genValidDataQueue(nbrItem: Int, genDataName: String, w: Int,
                        trueBias: Double = 1.0): Queue[(Boolean, BigInt)] = {
    val dataStream = genQueue(nbrItem, genDataName, w)
    val validStream = genBiasedBoolQueue(nbrItem, trueBias)
    validStream.zip(dataStream)
  }

  def genValidDataQueueWithNbrValid(nbrValid: Int, genDataName: String, w: Int,
                                    trueBias: Double = 1.0): Queue[(Boolean, BigInt)] = {
    val validStream = genBiasedBoolQueueWithNbrTrue(nbrValid, trueBias)
    val nbrItem     = validStream.length
    val dataStream  = genQueue(nbrItem, genDataName, w)
    validStream.zip(dataStream)
  }

  def genConcatDataQueue(nbrItemPerGroup: Int, nbrItem: Int,
                         genDataName: String, w: Int): Queue[BigInt] = {
    val datas = genQueue(nbrItem*nbrItemPerGroup, genDataName, w)
    val groupedDatas = datas.grouped(nbrItemPerGroup).toSeq
    // Want to preserve sign if nbrItem == 1, concatData removes it
    val datasConcat = if(nbrItemPerGroup >1) {groupedDatas.map{case x => concatData(x, w)}}
                      else {groupedDatas.map(x => x.head)}
    Queue(datasConcat: _*)
  }

  def genValidConcatDataQueueWithNbrValid(
      nbrDataPerItem: Int, nbrValid: Int, genDataName: String, w: Int,
      trueBias: Double = 1.0): Queue[(Boolean, BigInt)] = {
    val groupedValid = math.ceil(nbrValid.toDouble/nbrDataPerItem).toInt
    val validStream = genBiasedBoolQueueWithNbrTrue(groupedValid, trueBias)
    val nbrItem     = validStream.length
    val dataStream  = genConcatDataQueue(nbrDataPerItem, nbrItem, genDataName, w)
    validStream.zip(dataStream)
  }

  def keepOnlyValidInQueue(q: Queue[(Boolean, BigInt)]): Queue[BigInt] = {
    q.filter(_._1 == true).map(_._2) }

  def compareDataQueue(expectedQ: Queue[BigInt], actualQ: Queue[BigInt]): Queue[Boolean] = {
    expectedQ.zip(actualQ).map{ (vals) => vals._1 == vals._2 } }

  def assertCompareDataQueues(expectedQ: Queue[BigInt], actualQ: Queue[BigInt]): Unit = {
    val comparedQueue = compareDataQueue(expectedQ, actualQ)
    val allEqual = comparedQueue.reduce((a,b) => a && b)
    assert(allEqual, s"Compared queues are not equal:\n\texpected:${expectedQ}\n\tactual:${actualQ}\n")
  }

}
