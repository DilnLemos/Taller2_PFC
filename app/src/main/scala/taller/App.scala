/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package taller

object App {
  def main(args: Array[String]): Unit = {
    var conjuntos = new ConjuntosDifusos()
    var c1 = conjuntos.grande(10, 2)
    var c2 = conjuntos.grande(10, 3)
    for (n <- (1 to 20)) {
      println(c1(n) + " - " + c2(n))
    }
  }

  def greeting(): String = "Hello, world!"
}
