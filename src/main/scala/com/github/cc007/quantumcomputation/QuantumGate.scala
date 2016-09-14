package com.github.cc007.quantumcomputation

import breeze.linalg.{DenseMatrix, kron}
import breeze.math.Complex
import scala.math._

import utils.ComplexUtils._

/**
  * Created by Rik on 30-5-2016.
  */
object QuantumGate {
  implicit def matrixWrapper(matrix: DenseMatrix[Complex]): QuantumGate = new QuantumGate(matrix)

  implicit def transformationToMatrix(transformation: QuantumGate): DenseMatrix[Complex] = transformation.matrix

  def identity: QuantumGate = DenseMatrix.eye[Complex](2)

  def hadamard: QuantumGate = {
    val matrix = DenseMatrix.ones[Complex](2, 2)
    matrix(1, 1) = -1.0
    val mult: Complex = sqrt(1 / 2.0)
    matrix :*= mult
    matrix
  }

  def pauliX: QuantumGate = {
    val matrix = DenseMatrix.zeros[Complex](2, 2)
    matrix(0, 1) = 1.0
    matrix(1, 0) = 1.0
    matrix
  }

  def pauliY: QuantumGate = {
    val matrix = DenseMatrix.zeros[Complex](2, 2)
    matrix(0, 1) = -Complex.i
    matrix(1, 0) = Complex.i
    matrix
  }

  def pauliZ: QuantumGate = {
    val matrix = DenseMatrix.zeros[Complex](2, 2)
    matrix(0, 0) = 1.0
    matrix(1, 1) = -1.0
    matrix
  }

  def SWAP: QuantumGate = {
    val matrix = DenseMatrix.zeros[Complex](4, 4)
    matrix(0, 0) = 1.0
    matrix(1, 2) = 1.0
    matrix(2, 1) = 1.0
    matrix(3, 3) = 1.0
    matrix
  }

  def SWAP(qubitCount: Int, position: Int): QuantumGate = {
    var matrix: DenseMatrix[Complex] = SWAP
    for (_ <- 0 until position) {
      matrix = kron(identity.matrix, matrix)
    }
    for (_ <- 0 until qubitCount - 2 - position) {
      matrix = kron(matrix, identity.matrix)
    }
    matrix
  }

  def sqrtSWAP: QuantumGate = {
    val matrix = DenseMatrix.eye[Complex](4)
    matrix(1, 1) = 0.5 * (1.0 + Complex.i)
    matrix(1, 2) = 0.5 * (1.0 - Complex.i)
    matrix(2, 1) = 0.5 * (1.0 - Complex.i)
    matrix(2, 2) = 0.5 * (1.0 + Complex.i)
    matrix
  }

  def CNOT: QuantumGate = {
    val matrix = DenseMatrix.zeros[Complex](4, 4)
    matrix(0, 0) = 1.0
    matrix(1, 1) = 1.0
    matrix(2, 3) = 1.0
    matrix(3, 2) = 1.0
    matrix
  }

  def ControlledTF(oneBitTF: QuantumGate): QuantumGate = {
    val matrix = DenseMatrix.zeros[Complex](4, 4)
    matrix(0, 0) = 1.0
    matrix(1, 1) = 1.0
    matrix(2, 2) = oneBitTF.matrix(0, 0)
    matrix(2, 3) = oneBitTF.matrix(0, 1)
    matrix(3, 2) = oneBitTF.matrix(1, 0)
    matrix(3, 3) = oneBitTF.matrix(1, 1)
    matrix
  }

  def apply(qubitCount: Int, nBitTF: QuantumGate, positions: Int*): QuantumGate = {
    if (positions.nonEmpty && positions.length < nBitTF.qubitCount) throw new IllegalArgumentException("You didn't specify all qubit positions.")
    if (positions.length > nBitTF.qubitCount) throw new IllegalArgumentException("You specified too many qubit positions: " + positions.length + " > " + nBitTF.qubitCount)
    val identityCount = qubitCount - nBitTF.qubitCount
    var matrix = nBitTF.matrix.copy
    for (i <- 0 until identityCount) {
      matrix = kron(matrix, identity.matrix)
    }
    if (positions.nonEmpty) {
      var i = 0
      for (position <- positions) {
        //TODO optimize swapping
        for (j <- i until position) {
          matrix = SWAP(qubitCount, j).matrix * matrix
        }
        for (j <- position - 2 to i by -1) {
          matrix = SWAP(qubitCount, j).matrix * matrix
        }
        i += 1
      }
      i = 0
      for (position <- positions) {
        //TODO optimize swapping
        for (j <- i until position) {
          matrix = matrix * SWAP(qubitCount, j).matrix
        }
        for (j <- position - 2 to i by -1) {
          matrix = matrix * SWAP(qubitCount, j).matrix
        }
        i += 1
      }
    }
    matrix
  }

}

class QuantumGate(val qubitCount: Int) {
  if (qubitCount < 1) throw new IllegalArgumentException("You deed at least 1 qubit.")
  protected var _matrix = DenseMatrix.zeros[Complex](pow(2, qubitCount).toInt, pow(2, qubitCount).toInt)

  def matrix = _matrix

  def this(matrix: DenseMatrix[Complex]) {
    this((log(matrix.cols) / log(2)).toInt)
    if (matrix.cols != matrix.rows) throw new IllegalArgumentException("The matrix is not square.")
    _matrix = matrix
  }

  def transform(matrixTransformation: QuantumGate): Unit = {
    val b: DenseMatrix[Complex] = matrixTransformation
    _matrix *= b
  }

  def this(matrices: List[DenseMatrix[Complex]], qubitCount: Int) {
    this(qubitCount)
    var resultMatrix = matrices.head
    for (i <- 1 until matrices.length) {
      resultMatrix = kron(resultMatrix, matrices(i))
    }
    _matrix = resultMatrix
  }

  override def toString: String = toString(2) //_matrix.toString()

  def toString(precision: Int = 2, showImag:Boolean = true): String = {
    var string = ""
    for (i <- 0 until matrix.rows) {
      string += "|"
      for (j <- 0 until matrix.cols) {
        string += (("% 1." + precision + "f") format matrix(i, j).real)
        if(showImag) {
          if (matrix(i, j).imag < 0.0) {
            string += " -"
          } else {
            string += " +"
          }
          string += (("% 1." + precision + "f") format matrix(i, j).imag) + "i "
        }else{
          string += " "
        }
      }
      string += "|\n"
    }
    string
  }
}

