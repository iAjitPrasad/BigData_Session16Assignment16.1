class appRationalCalc (inp1: Int, inp2: Int) {

  require(inp2 != 0)
  var numerator = 0
  var denominator = 0

  /***Find GCD of input provided.
	This gives us numerator & denominator.***/
  private def findGCD(a:Int, b:Int): Int =
    if(b == 0) a
    else findGCD(b, a % b)

  if (inp2 != 0) {
    val getGCD = findGCD (inp1.abs, inp2.abs)
    numerator = inp1 / getGCD
    denominator = inp2 / getGCD
  }

  /***Auxiliary construct***/
  def this(n: Int) = this(n, 1)

  /***Addition operations on Rational and Whole Numbers***/
  def + (that: appRationalCalc): appRationalCalc =
    new appRationalCalc(numerator * that.denominator + that.numerator * denominator, denominator * that.denominator)

  /***Method overloading for "+" in order to perform a addition***/
  def + (i: Int): appRationalCalc =
    new appRationalCalc(numerator + i * denominator, denominator)

  /***Subtraction operations on Rational and Whole Numbers***/
  def - (that: appRationalCalc): appRationalCalc =
    new appRationalCalc( numerator * that.denominator - that.numerator * denominator, denominator * that.denominator)

  /***Method overloading for "-" in order to perform a subtraction***/
  def - (i: Int): appRationalCalc =
    new appRationalCalc(numerator - i * denominator, denominator)

  /***Multiplication operations on Rational and Whole Numbers***/
  def * (that: appRationalCalc): appRationalCalc =
    new appRationalCalc(numerator * that.numerator, denominator * that.denominator)

  /***Method overloading for "*" in order to perform a multiplication***/
  def * (i: Int): appRationalCalc =
    new appRationalCalc(numerator * i, denominator)

  /***Division operations on Rational and Whole Numbers***/
  def / (that: appRationalCalc): appRationalCalc =
    new appRationalCalc(numerator * that.denominator, denominator * that.numerator)

  /***Method overloading for "/" in order to perform a division***/
  def / (i: Int): appRationalCalc =
    new appRationalCalc(numerator, denominator * i)

  /***To display the output in the format "n/d"
	we have to override as we are overloading functions using auxiliary constructor***/
  override def toString = numerator + "/" + denominator
}

object appRatlCalc {

  /***Display list of operations that can be performed by the user***/
  private def OperationsList() = {

    println("Rational Calculator")
    println("___________________")

    println("Pick an operation to perform")
    println("1. Addition")
    println("2. Subtraction")
    println("3. Multiplication")
    println("4. Division")
    println("5. Add a Rational number with an Integer")
    println("6. Subtract a Rational number with an Integer")
    println("7. Multiply a Rational number with an Integer")
    println("8. Divide a Rational number with an Integer")
    println("9. Exit")
  }

  /***Get input and call appropriate overloaded method in class based on user's choice***/
  def Compute(rational: appRationalCalc, number: Int): appRationalCalc = {

    number match {
    case  1  =>
    val n = scala.io.StdIn.readInt()
    val d = scala.io.StdIn.readInt()
    rational.+(new appRationalCalc(n, d))

    case  2  =>
    val n = scala.io.StdIn.readInt()
    val d = scala.io.StdIn.readInt()
    rational.-(new appRationalCalc(n, d))

    case  3  =>
    val n = scala.io.StdIn.readInt()
    val d = scala.io.StdIn.readInt()
    rational.*(new appRationalCalc(n, d))

    case  4  =>
    val n = scala.io.StdIn.readInt()
    val d = scala.io.StdIn.readInt()
    rational./(new appRationalCalc(n, d))

    case  5  =>
    val n = scala.io.StdIn.readInt()
    rational.+(new appRationalCalc(n))

    case  6  =>
    val n = scala.io.StdIn.readInt()
    rational.-(new appRationalCalc(n))

    case  7  =>
    val n = scala.io.StdIn.readInt()
    rational.*(new appRationalCalc(n))

    case  8  =>
    val n = scala.io.StdIn.readInt()
    rational./(new appRationalCalc(n))

    case  _  => rational
    }
  }

  /***Driver Method***/
  def main(args: Array[String]): Unit = {

    /***Create instance of class***/
    var ratlNumb: appRationalCalc = new appRationalCalc(0)

    var input = 0

    do {
      OperationsList()
      input = scala.io.StdIn.readInt()

      ratlNumb = Compute(ratlNumb, input)

      println("____________________")
      println("OUTPUT: " + ratlNumb.toString) /***Result of the Calculation***/
      println("____________________\n")
    }
    while (input != 9)
  }
}