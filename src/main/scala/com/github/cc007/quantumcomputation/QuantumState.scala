package com.github.cc007.quantumcomputation

import scala.collection.mutable.ListBuffer

import breeze.linalg.{DenseMatrix, DenseVector, kron}
import breeze.math.Complex
import scala.math._

import utils.ComplexUtils._

/**
  * Created by Rik on 30-5-2016.
  */
object QuantumState {
  implicit def vectorWrapper(vector: DenseVector[Complex]): QuantumState = new QuantumState(vector)

  implicit def stateToVector(state: QuantumState): DenseVector[Complex] = state.vector

  implicit def stateListToVectorList(states: List[QuantumState]): List[DenseVector[Complex]] = {
    val vectors: ListBuffer[DenseVector[Complex]] = ListBuffer()
    for (state <- states) {
      vectors += state
    }
    vectors.toList
  }

}

class QuantumState(val qubitCount: Int) {
  if (qubitCount < 1) throw new IllegalArgumentException("You deed at least 1 qubit.")
  protected var _vector = DenseVector.zeros[Complex](pow(2, qubitCount).toInt)
  _vector(0) = 1.0

  def this(vector: DenseVector[Complex]) {
    this(vector.length)
    this._vector = vector
  }

  def this(vectors: List[DenseVector[Complex]], qubitCount: Int) {
    this(qubitCount)
    var resultMatrix = vectors.head.toDenseMatrix
    for (i <- 1 until vectors.length) {
      resultMatrix = kron(resultMatrix, vectors(i).toDenseMatrix)
    }
    _vector = resultMatrix.toDenseVector
  }

  def vector = _vector

  def transform(matrixTransformation: QuantumGate): Unit = {
    val b: DenseMatrix[Complex] = matrixTransformation
    this._vector = (this._vector.toDenseMatrix * b).toDenseVector
  }

  override def toString: String = toString(2) //_vector.toString()

  def toString(precision: Int): String = {
    var string = ""
    for (nr <- vector) {
      string += "|" + (("% 1." + precision + "f") format nr.real)
      if (nr.imag < 0.0) {
        string += " -"
      } else {
        string += " +"
      }
      string += (("% 1." + precision + "f") format nr.imag) + "i |\n"
    }
    string
  }

  def copy:QuantumState = vector.copy
}
