package taller

import scala.annotation.tailrec

class ConjuntosDifusos {

    type ConjDifuso = Int => Double 
    
    
    def pertenece (elem: Int, s: ConjDifuso): Double = {

        s (elem)

    }


    def muchoMayorQue(a: Int, m: Int): ConjDifuso = {
        def mma(x: Int): Double = {
            if (x <= a) 0.0
            else if (x >= m) 1.0
            else (x - a).toDouble / (m - a).toDouble
        }
        mma
    }


    def grande(d: Int, c: Int): ConjDifuso = {
        require(c > 1, "c debe ser mayor que 1")
        x => math.pow(x.toDouble / (x.toDouble + d), c)
    }


        // DE AQUI PARA ABAJO FUE PURO TABNINE XD
    def complemento (c: ConjDifuso): ConjDifuso = {

        x => 1 - c(x)

    }


    def union (cd1: ConjDifuso, cd2: ConjDifuso): ConjDifuso = {
        // haremos la función de union de los conjuntos difusos
        x => math.max(cd1(x), cd2(x))
    }


    def interseccion (cd1: ConjDifuso, cd2: ConjDifuso): ConjDifuso = {
        // haremos la función de intersección de los conjuntos difusos
        x => math.min(cd1(x), cd2(x))
    }

    def inclusion(cd1: ConjDifuso, cd2: ConjDifuso) : Boolean = {
        @tailrec
        def testRange(lb: Int, ub: Int, cd1: ConjDifuso, cd2: ConjDifuso): Boolean = {
        if (!(cd1(lb) <= cd2(lb)))  false
        else if (lb == ub) true
        else testRange(lb+1, ub, cd1, cd2)
        }
        testRange(1, 1000, cd1, cd2)
    }

    def igualdad(cd1: ConjDifuso, cd2: ConjDifuso) : Boolean = {
        inclusion(cd1, cd2) && inclusion(cd2, cd1) 
    }
}
