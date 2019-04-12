package fpgabits.testUtils

import scala.language.reflectiveCalls
import chisel3._
import chisel3.experimental._
import chisel3.iotesters._
import chisel3.core.BaseModule
import scala.collection.mutable._


class FPGABitsFlatSpec extends ChiselFlatSpec{
  val backendName  = "firrtl"
  val verbose  = true
  val saveWave = true
  val moduleName = "DefaultModuleName"

  def getBackendConfigArray(behaviour: String, confIdx: Int=0): Array[String] = {
    val config = ArrayBuffer[String]()
    config ++= Array("--backend-name", backendName)
    def rBadChar(s: String): String = s.replaceAll("\\s+", "").replaceAll(",", "_").replaceAll("'", "")
    if (verbose)  { config += "--is-verbose" }
    if (saveWave) {
      config ++= Array("-tn", moduleName,
                       "-td", s"testOutput/${rBadChar(moduleName)}/${rBadChar(behaviour)}_${confIdx}")}
    if(verbose){
      config.foreach{x => print(s"${x} ")}
      print("\n") }
    config.toArray
  }

  def execBehaviour[M <: MultiIOModule, T <: PeekPokeTester[M]](
    behaviour: String, moduleGen: () => M, behaviourGen: (M) => T,
    confIdx: Int = 0, bhvEnable: Boolean = false){
    val configArray = getBackendConfigArray(behaviour, confIdx)
    if (bhvEnable){
      it should s"${behaviour} with conf${confIdx}" in {
        iotesters.Driver.execute(configArray, moduleGen) {
          behaviourGen
        } should be (true)
      }
    } else {
      ignore should s"${behaviour} with conf${confIdx}" in {
        iotesters.Driver.execute(configArray, moduleGen) {
          behaviourGen
        } should be (true)
      }
    }
  }

  def execBehaviourList[M <: MultiIOModule, T <: PeekPokeTester[M]](
    behaviourList: List[((M) => T, String)], bhvEnable: List[Boolean],
    moduleGen: () => M, confIdx: Int) {
    if(behaviourList.length != bhvEnable.length){
      throw new IllegalArgumentException(
        s"Behaviour List length (${behaviourList.length})"+
        s" should match the enable list (${bhvEnable.length})" +
        s"\n***** Have you forgotten a true/false in the 'testEnable' value!? *****")
    }
    behaviourList.zipWithIndex.foreach {
      tuple => {
        val bhvGen  = tuple._1._1
        val bhvDesc = tuple._1._2
        val idx     = tuple._2
        execBehaviour[M, T](bhvDesc, moduleGen, bhvGen, confIdx, bhvEnable(idx))
      }
    }
  }
}
