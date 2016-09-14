package com.github.cc007.quantumcomputation.utils

import breeze.linalg.{DenseMatrix, Matrix}
import breeze.linalg.operators.{BinaryRegistry, OpMulScalar}
import breeze.math.Complex

/**
  * Created by Rik on 13-6-2016.
  */
object ComplexUtils {

  implicit def doubleToComplex(d: Double): Complex = new Complex(d, 0.0)

  implicit def s_dm_op_Complex_OpMulScalar(implicit op: OpMulScalar.Impl2[Complex, Complex, Complex]):
  OpMulScalar.Impl2[Complex, DenseMatrix[Complex], DenseMatrix[Complex]] =

    new OpMulScalar.Impl2[Complex, DenseMatrix[Complex], DenseMatrix[Complex]] {
      def apply(b: Complex, a: DenseMatrix[Complex]): DenseMatrix[Complex] = {
        val res: DenseMatrix[Complex] = DenseMatrix.zeros[Complex](a.rows, a.cols)
        val resd: Array[Complex] = res.data
        val ad: Array[Complex] = a.data
        var c = 0

        var off = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            resd(off) = op(b, ad(a.linearIndex(r, c)))
            r += 1
            off += 1
          }
          c += 1
        }

        res
      }

      implicitly[BinaryRegistry[Complex, Matrix[Complex], OpMulScalar.type, Matrix[Complex]]].register(this)
    }

}
