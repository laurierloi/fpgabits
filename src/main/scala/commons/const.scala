package fpgabits.commons
import chisel3._
import chisel3.util.Enum
import fpgabits.core._

// No magic number scalastyle for this file, its role is to instantiate magic numbers
// scalastyle:off magic.number

object  Const {
  /* Definition of constants types for fpgabits */

  object constMuxGenMAcc{
    val allMux     = Seq(true,  true,  true)
    val noMux      = Seq(false, false, false)
    val onlyPsum   = Seq(false, false, true)
    val onlyWeight = Seq(true,  false, false)
    val onlyAct    = Seq(false, true,  false)
  }
  object constMuxGenPassThrough{
    val allMux     = Seq(true,  true,  true)
    val noMux      = Seq(false, false, false)
    val onlyPsum   = Seq(false, false, true)
    val onlyWeight = Seq(true,  false, false)
    val onlyAct    = Seq(false, true,  false)
  }

  object constBRAM{
    /** From table 1-7,1-8,1-9,1-10 in UG573
      * Nomenclature: 18 or 36 for BRAM type
      *               S/T,  for (S)imple/(T)rue Dual Port
      *               Dn, where n is data width
      *               An, where n is address width
      *    (optional) Wn, where n is the alternate port witdth(only for SDP)
      *    (optional) C,  for cascade implementation
      */

    /* Fix the index of the write and read ports for the sdp */
    val sdpWritePort :: sdpReadPort = Enum(2)

    /* PORT PARAM DEFINITIONS */
    /* 18 TDP */
    //  (addrWidth, dataWidth, size)
    val BRAM18TD1A14     = BRAMPortParam(14, 1,  16384)
    val BRAM18TD2A13     = BRAMPortParam(13, 2,  8192)
    val BRAM18TD4A12     = BRAMPortParam(12, 4,  4096)
    val BRAM18TD9A11     = BRAMPortParam(11, 9,  2048)
    val BRAM18TD18A10    = BRAMPortParam(10, 18, 1024)

    /* 36 TDP */
    val BRAM36TD1A15     = BRAMPortParam(15, 1,  32768)
    val BRAM36TD2A14     = BRAMPortParam(14, 2,  16384)
    val BRAM36TD4A13     = BRAMPortParam(13, 4,  8192)
    val BRAM36TD9A12     = BRAMPortParam(12, 9,  4096)
    val BRAM36TD18A11    = BRAMPortParam(11, 18, 2048)
    val BRAM36TD36A10    = BRAMPortParam(10, 36, 1024)
    //val BRAM36TD1A16C    = BRAMPortParam(16, 1,  65546)

    // Dict key is the data width
    val TDPBRAM18AvailableWidths = Seq(1, 2, 4, 9, 18)
    val TDPBRAM36AvailableWidths = Seq(1, 2, 4, 9, 18, 36)
    val TDPBRAM18ParamDict = Map[Int , BRAMPortParam](
      1 -> BRAM18TD1A14 , 2   -> BRAM18TD2A13  , 4  -> BRAM18TD4A12 ,
      9 -> BRAM18TD9A11 , 18  -> BRAM18TD18A10)
    val TDPBRAM36ParamDict = Map[Int , BRAMPortParam](
      1 -> BRAM36TD1A15 , 2   -> BRAM36TD2A14  , 4  ->  BRAM36TD4A13 ,
      9 -> BRAM36TD9A12 , 18  -> BRAM36TD18A11 , 36 -> BRAM36TD36A10)

    def getTDPClosestWidth(width: Int, isBRAM18: Boolean): Int = {
      if(isBRAM18){ TDPBRAM18AvailableWidths.filter(_ >= width)(0) }
      else {        TDPBRAM36AvailableWidths.filter(_ >= width)(0) } }
    def getTDPBRAMPortParam(width: Int, isBRAM18: Boolean): BRAMPortParam = {
      if(isBRAM18) { TDPBRAM18ParamDict(width) }
      else         { TDPBRAM36ParamDict(width) } }


    /* 18 SDP */
    val BRAM18SD32A14W1  = new BRAMConfig(false, Seq(9, 14), Seq(32, 1 ), Seq(512, 16384))
    val BRAM18SD32A13W2  = new BRAMConfig(false, Seq(9, 13), Seq(32, 2 ), Seq(512, 8192 ))
    val BRAM18SD32A12W4  = new BRAMConfig(false, Seq(9, 12), Seq(32, 4 ), Seq(512, 4096 ))
    val BRAM18SD36A11W9  = new BRAMConfig(false, Seq(9, 11), Seq(36, 9 ), Seq(512, 2048 ))
    val BRAM18SD36A10W18 = new BRAMConfig(false, Seq(9, 10), Seq(36, 18), Seq(512, 1024 ))
    val BRAM18SD36A10W36 = new BRAMConfig(false, Seq(9, 10), Seq(36, 36), Seq(512, 512  ))

    /* 36 SDP */
    val BRAM36SD64A15W1  = new BRAMConfig(false, Seq(9, 15), Seq(64, 1 ), Seq(512, 32768))
    val BRAM36SD64A14W2  = new BRAMConfig(false, Seq(9, 14), Seq(64, 2 ), Seq(512, 16384))
    val BRAM36SD64A13W4  = new BRAMConfig(false, Seq(9, 13), Seq(64, 4 ), Seq(512, 8192 ))
    val BRAM36SD72A12W9  = new BRAMConfig(false, Seq(9, 12), Seq(72, 9 ), Seq(512, 4096 ))
    val BRAM36SD72A11W18 = new BRAMConfig(false, Seq(9, 11), Seq(72, 18), Seq(512, 2048 ))
    val BRAM36SD72A10W36 = new BRAMConfig(false, Seq(9, 10), Seq(72, 36), Seq(512, 1024 ))
    val BRAM36SD72A9W72  = new BRAMConfig(false, Seq(9, 9 ), Seq(72, 72), Seq(512, 512  ))

    val SDPBRAM18AvailableWidths = Seq(1, 2, 4, 9, 18, 36)
    val SDPBRAM36AvailableWidths = Seq(1, 2, 4, 9, 18, 36, 72)
    val SDPBRAM18ConfigDict = Map[Int, BRAMConfig](
        1  -> BRAM18SD32A14W1, 2  -> BRAM18SD32A13W2, 4  -> BRAM18SD32A12W4,
        9  -> BRAM18SD36A11W9, 18 -> BRAM18SD36A10W18, 36 -> BRAM18SD36A10W36)
    val SDPBRAM36ConfigDict = Map[Int, BRAMConfig](
      1  -> BRAM36SD64A15W1 , 2  -> BRAM36SD64A14W2 , 4  -> BRAM36SD64A13W4 ,
      9  -> BRAM36SD72A12W9 , 18 -> BRAM36SD72A11W18, 36 -> BRAM36SD72A10W36,
      72 -> BRAM36SD72A9W72)

  }
}
  // scalastyle:on magic.number
