package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ConjuntosDifusosTest extends AnyFunSuite {
    val objConjuntosDifusos = new ConjuntosDifusos()

  // Tests para pertenece
    test("pertenece: elemento 5 en muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    assert(math.abs(objConjuntosDifusos.pertenece(5, conjunto) - 0.4444444444444444) < 0.000001)
    }

    test("pertenece: elemento 0 en muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    assert(objConjuntosDifusos.pertenece(0, conjunto) == 0.0)
    }

    test("pertenece: elemento 15 en muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    assert(objConjuntosDifusos.pertenece(15, conjunto) == 1.0)
    }

    test("pertenece: elemento 50 en grande(10, 2)") {
    val conjunto = objConjuntosDifusos.grande(10, 2)
    assert(math.abs(objConjuntosDifusos.pertenece(50, conjunto) - 0.8333333333333334) < 0.000001)
    }

    test("pertenece: elemento 5 en grande(10, 2)") {
    val conjunto = objConjuntosDifusos.grande(10, 2)
    assert(math.abs(objConjuntosDifusos.pertenece(5, conjunto) - 0.1111111111111111) < 0.000001)
    }

  // Tests para grande
    test("grande: 50 con (10, 2)") {
    val conjunto = objConjuntosDifusos.grande(10, 2)
    assert(math.abs(conjunto(50) - 0.8333333333333334) < 0.000001)
    }

    test("grande: 5 con (10, 2)") {
    val conjunto = objConjuntosDifusos.grande(10, 2)
    assert(math.abs(conjunto(5) - 0.1111111111111111) < 0.000001)
    }

    test("grande: 100 con (10, 2)") {
    val conjunto = objConjuntosDifusos.grande(10, 2)
    assert(math.abs(conjunto(100) - 0.9090909090909091) < 0.000001)
    }

    test("grande: 0 con (10, 2)") {
    val conjunto = objConjuntosDifusos.grande(10, 2)
    assert(conjunto(0) == 0.0)
    }

    test("grande: 1000 con (10, 2)") {
    val conjunto = objConjuntosDifusos.grande(10, 2)
    assert(math.abs(conjunto(1000) - 0.9900990099009901) < 0.000001)
    }

  // Tests para complemento
    test("complemento: 5 en complemento de muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    val complementoConjunto = objConjuntosDifusos.complemento(conjunto)
    assert(math.abs(complementoConjunto(5) - 0.5555555555555556) < 0.000001)
    }

    test("complemento: 1 en complemento de muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    val complementoConjunto = objConjuntosDifusos.complemento(conjunto)
    assert(complementoConjunto(1) == 1.0)
    }

    test("complemento: 10 en complemento de muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    val complementoConjunto = objConjuntosDifusos.complemento(conjunto)
    assert(complementoConjunto(10) == 0.0)
    }

    test("complemento: 0 en complemento de muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    val complementoConjunto = objConjuntosDifusos.complemento(conjunto)
    assert(complementoConjunto(0) == 1.0)
    }

    test("complemento: 15 en complemento de muchoMayorQue(1, 10)") {
    val conjunto = objConjuntosDifusos.muchoMayorQue(1, 10)
    val complementoConjunto = objConjuntosDifusos.complemento(conjunto)
    assert(complementoConjunto(15) == 0.0)
    }

  // Tests para union
    test("union: 5 en union de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val unionConjunto = objConjuntosDifusos.union(conjunto1, conjunto2)
    assert(math.abs(unionConjunto(5) - 0.4444444444444444) < 0.000001)
    }

    test("union: 1 en union de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val unionConjunto = objConjuntosDifusos.union(conjunto1, conjunto2)
    assert(math.abs(unionConjunto(1) - 0.009090909090909092) < 0.000001)
    }

    test("union: 10 en union de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val unionConjunto = objConjuntosDifusos.union(conjunto1, conjunto2)
    assert(unionConjunto(10) == 1.0)
    }

    test("union: 0 en union de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val unionConjunto = objConjuntosDifusos.union(conjunto1, conjunto2)
    assert(unionConjunto(0) == 0.0)
    }

    test("union: 100 en union de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val unionConjunto = objConjuntosDifusos.union(conjunto1, conjunto2)
    assert(unionConjunto(100) == 1.0)
    }

  // Tests para interseccion
    test("interseccion: 5 en interseccion de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val interseccionConjunto = objConjuntosDifusos.interseccion(conjunto1, conjunto2)
    assert(math.abs(interseccionConjunto(5) - 0.1111111111111111) < 0.000001)
    }

    test("interseccion: 1 en interseccion de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val interseccionConjunto = objConjuntosDifusos.interseccion(conjunto1, conjunto2)
    assert(interseccionConjunto(1) == 0.0)
    }

    test("interseccion: 10 en interseccion de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val interseccionConjunto = objConjuntosDifusos.interseccion(conjunto1, conjunto2)
    assert(math.abs(interseccionConjunto(10) - 0.5) < 0.000001)
    }

    test("interseccion: 0 en interseccion de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val interseccionConjunto = objConjuntosDifusos.interseccion(conjunto1, conjunto2)
    assert(interseccionConjunto(0) == 0.0)
    }

    test("interseccion: 100 en interseccion de muchoMayorQue(1, 10) y grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    val interseccionConjunto = objConjuntosDifusos.interseccion(conjunto1, conjunto2)
    assert(math.abs(interseccionConjunto(100) - 0.9090909090909091) < 0.000001)
    }

  // Tests para inclusion
    test("inclusion: muchoMayorQue(1, 10) incluido en muchoMayorQue(1, 20)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 10)
    val conjunto2 = objConjuntosDifusos.muchoMayorQue(1, 20)
    assert(objConjuntosDifusos.inclusion(conjunto1, conjunto2))
    }

    test("inclusion: muchoMayorQue(1, 20) no incluido en muchoMayorQue(1, 10)") {
    val conjunto1 = objConjuntosDifusos.muchoMayorQue(1, 20)
    val conjunto2 = objConjuntosDifusos.muchoMayorQue(1, 10)
    assert(!objConjuntosDifusos.inclusion(conjunto1, conjunto2))
    }

    test("inclusion: grande(10, 2) incluido en grande(10, 3)") {
    val conjunto1 = objConjuntosDifusos.grande(10, 2)
    val conjunto2 = objConjuntosDifusos.grande(10, 3)
    assert(objConjuntosDifusos.inclusion(conjunto1, conjunto2))
    }

    test("inclusion: grande(10, 3) no incluido en grande(10, 2)") {
    val conjunto1 = objConjuntosDifusos.grande(10, 3)
    val conjunto2 = objConjuntosDifusos.grande(10, 2)
    assert(!objConjuntosDifusos.inclusion(conjunto1, conjunto2))
    }
}