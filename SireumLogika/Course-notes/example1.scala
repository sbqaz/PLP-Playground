import org.sireum.logika._
// the above imports, for example, type Z,
// which is an arbitrary-precision integer type (i.e., scala.BigInt)
// for the remaining examples, we'll elide it

val x: Z = readInt() // readInt asks an integer from the user via console input
val y: Z = readInt() // val declares a read-only variable
var max: Z = 0       // var declares a read/write variable
if (x > y) {
  max = x
} else {
  max = y
}
println("Maximum of ", x, " and ", y, " is ", max, ".")