package org.vricsa.fixedPoint

class FixedPoint {
  var Q : Int = 0;
  var F : Int = 0
  var TotalBits :  Int =0
  var bitMask : Int = 0
  var msbCheck : Int = 0

  // Define the Quotionet and Fraction
  def setQ(totalbits:Int,q:Int):Unit = {
    TotalBits = totalbits
    Q = q
    F = TotalBits - Q

    msbCheck = 1 << TotalBits - 1


    for (i <- 1 to totalbits) {
      //println(i)
      bitMask = (bitMask << 1) + 1
    }
  }
  def fixAdd(inA:Int,inB:Int): (Int,Double) = {
    var fixaddition: Int = 0
    //fixaddition = (inA + inB) & bitMask
    //println("Add values are " + inA + " B =  " + inB)
    fixaddition = (inA + inB)
    //println("The Add is "+(fixaddition).toBinaryString)
    return (fixaddition & bitMask,fixToFloat(fixaddition))
  }

  def fixSub(inA: Int, inB: Int): (Int, Double) = {
    var fixaddition: Int = 0
    //println("Sub values are " + inA +" B = " + inB)
    fixaddition = (inA - inB) // & bitMask
    return (fixaddition & bitMask , fixToFloat(fixaddition))
  }

  def fixMul(inA: Int, inB: Int): (Int, Double) = {
    var fixaddition: Int = 0
    fixaddition = ((inA * inB) >> F) // & bitMask
    return (fixaddition& bitMask, fixToFloat(fixaddition))
  }

  def fixDiv(inA: Int, inB: Int): (Int, Double) = {
    var fixaddition: Int = 0
    fixaddition = ((inA<<F) / inB) //& bitMask
    return ((fixaddition & bitMask), fixToFloat(fixaddition))
  }
  def floatToFixSub(inA: Double, inB: Double): (Int, Double) = {
    var fixA: Int = 0
    var fixB: Int = 0
    fixA = floatToFix(inA)
    fixB = floatToFix(inB)
    return fixSub(fixA, fixB)
  }
  def floatToFixAdd(inA:Double,inB:Double): (Int,Double) = {
    var fixA : Int = 0
    var fixB : Int = 0
    fixA = floatToFix(inA)
    fixB = floatToFix(inB)
    return fixAdd(fixA,fixB)
  }

  def floatToFixMul(inA: Double, inB: Double): (Int, Double) = {
    var fixA: Int = 0
    var fixB: Int = 0
    fixA = floatToFix(inA)
    fixB = floatToFix(inB)
    return fixMul(fixA, fixB)
  }

  def floatToFixDiv(inA: Double, inB: Double): (Int, Double) = {
    var fixA: Int = 0
    var fixB: Int = 0
    fixA = floatToFix(inA)
    fixB = floatToFix(inB)
    return fixDiv(fixA, fixB)
  }
  def floatToFix(inData:Double): Int = {
    var nu : Int = 0
    var frac : Int = 0
    var t : Double = 0.0
    val pw = math.pow(2,Q).toInt
    nu = (inData/(1.0)).toInt
    t = (inData%(1.0))
    var tf : Double = 0.0
    var shiftVal : Int=0
    nu = nu << F
    //println("The Integer = "+nu +"Fraction = "+t)
    tf = t
    var q : Int = 0
    for(i <- 1 to F){
      tf = tf * 2.0
      q = (tf/(1.0)).toInt
      tf = tf%1.0
      if(q == 1) {
        shiftVal = (shiftVal << 1) + 1
      }
      else
      {
        shiftVal = shiftVal << 1
      }
    }
    //println("The Final value ="+(nu+shiftVal).toBinaryString + " and dec = " + (nu+shiftVal))
    return  nu+shiftVal
  }


  def fixToFloat(fixValue:Int):Double = {
    var Frac : Double = 0.0
    var intVal : Double =0.0
    var fAndVal : Int = 0
    var numInt : Int = 0
    //fAndVal = fixSignExtn(fixValue)
    fAndVal = fixValue

    for (i <- 1 to F) {
      if((fAndVal&(1<<(F-i))) != 0){
        Frac = Frac +  math.pow(2,-i)
      }
    }

    numInt = fAndVal >> F
    if (numInt > math.pow(2,Q-1))
    {

      numInt = numInt ^ (bitMask >> F)
      numInt = numInt * -1
      println("The result is negative " + numInt)
    }

    intVal = (fAndVal >> F)*1.0 // For fraction part


    //println("The value of FixtoFloat is " + (intVal+Frac))
    return intVal+Frac
  }
  def fixSignExtn(inData:Int): Int = {
    // Unsigned number to signed number

    // Find the MSB bit if it set to one then take twos complement and multiply - to it
    var signedResult : Int = 0

    if((inData & msbCheck)> 0){
      // MSB is one
      // println("Inside for negative number")
      signedResult = ~inData + 1 // Twos complement
      //println("Signed value = " + signedResult)
      //signedResult = signedResult * -1 // Adding the minus sign
    }
    else
      signedResult = inData

    return signedResult
  }
}
object FixedPointTester {
  val s = new FixedPoint()
  def main(args: Array[String]): Unit = {
    var m:Int = 0
    s.setQ(16,8)
    m = s.floatToFix(12.15258)
    s.fixToFloat(fixValue = m)
    var val1:Int = 0
    var val2 :Double = 0.0
    var tupValues = s.floatToFixAdd(-13.65,3.25)
    println(" Addition = " + s.floatToFixAdd(-13.65,3.25))
    println("Subtraction = " + s.floatToFixSub(-13.25,3.25))
    println("Multiplication = " + s.floatToFixMul(1.50,3.5))
    println("Division = " + s.floatToFixDiv(120.75,1.5))
    println("The Fix value mask is "+s.fixSignExtn(tupValues._1))
  }
}