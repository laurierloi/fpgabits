package fpgabits.interfaces

import scala.language.reflectiveCalls
import chisel3._
import chisel3.util._

abstract class AxiStreamSignals[+T <: Data](private val gen: T, val withTLast: Boolean,
                                            val withTKeep: Boolean) extends Bundle{
  val tready = Output(Bool())
  val tvalid = Input(Bool())
  val tdata  = Input(gen)
  val tlast  = if(withTLast) Some(Input(Bool())) else None
  def getTKeepWidth: Int = gen.getWidth/8
  val tkeep  = if(withTKeep) Some(Input(UInt(getTKeepWidth.W))) else None
}

object AxiStreamSignals {
  implicit class AddMethodsToAxiStream[T <: Data](target: AxiStreamSignals[T]){

    def toDecoupled: DecoupledIO[T] = {
      val enq = Wire(EnqIO(target.tdata.cloneType))
      target.tready := enq.ready
      enq.valid     := target.tvalid
      enq.bits      := target.tdata
      enq
    }

    def fire          : Bool = target.tready &&  target.tvalid
    def validNotReady : Bool = target.tvalid && !target.tready
    def readyNotValid : Bool = target.tready && !target.tvalid

    def andValidCondition(cond: Bool) : AxiStreamSignals[T] = {
      val localAxi = Wire(target.cloneType)
      localAxi.tdata := target.tdata
      localAxi.tvalid := target.tvalid && cond
      if(target.withTLast) {localAxi.tlast.get := target.tlast.get}
      if(target.withTKeep) {localAxi.tkeep.get := target.tkeep.get}
      target.tready  := localAxi.tready
      localAxi
    }

    // TODO: add support of queue with bundled up TLast and TKeep
    def queue(entries: Int) : AxiStreamSignals[T] = {
      assert(target.withTLast == false, "AxiStreamSignals Queue doesn't support tlast")
      assert(target.withTKeep == false, "AxiStreamSignals Queue doesn't support tkeep")
      val axiOut = Wire(target.cloneType)
      val queue = Queue(target.toDecoupled, entries)
      axiOut.tdata  := queue.bits
      axiOut.tvalid := queue.valid
      queue.ready   := axiOut.tready
      axiOut
    }

  }
}

class AxiStreamIO[+T <: Data](gen: T, withTLast: Boolean, withTKeep: Boolean)
    extends AxiStreamSignals(gen, withTLast, withTKeep){
  override def cloneType: this.type = new AxiStreamIO(gen, withTLast, withTKeep).asInstanceOf[this.type]
}

object AxiStream{
  def apply[T <: Data](gen: T): AxiStreamSignals[T]                = new AxiStreamIO(gen, false, false)
  def apply[T <: Data](gen: T, last: Boolean): AxiStreamSignals[T] = new AxiStreamIO(gen, last, false)
  def apply[T <: Data](gen: T, last: Boolean, keep: Boolean): AxiStreamSignals[T] =
    new AxiStreamIO(gen, last, keep)

}

object AxiStreamSlave{
  def apply[T <: Data](gen: T): AxiStreamSignals[T]                = AxiStream(gen)
  def apply[T <: Data](gen: T, last: Boolean): AxiStreamSignals[T] = AxiStream(gen, last)
  def apply[T <: Data](gen: T, last: Boolean, keep: Boolean): AxiStreamSignals[T] =
    AxiStream(gen, last, keep)
}

object AxiStreamMaster{
  def apply[T <: Data](gen: T): AxiStreamSignals[T]                = Flipped(AxiStream(gen))
  def apply[T <: Data](gen: T, last: Boolean): AxiStreamSignals[T] = Flipped(AxiStream(gen, last))
  def apply[T <: Data](gen: T, last: Boolean, keep: Boolean): AxiStreamSignals[T] =
    Flipped(AxiStream(gen, last, keep))
}
