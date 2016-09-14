package com.github.cc007.quantumcomputation.demos

import com.github.cc007.quantumcomputation.{QuantumGate, QuantumState}

/**
  * Created by Rik on 30-5-2016.
  */
object App {
  def main(args: Array[String]) {
    val v1 = new QuantumState(1)
    println(v1)
    val v2 = new QuantumState(1)
    println(QuantumGate.hadamard)
    v2.transform(QuantumGate.hadamard)
    println(v2)
    val v12 = new QuantumState(List(v2, v1), 2)
    println(v12)
    println(QuantumGate.CNOT)
    v12.transform(QuantumGate.CNOT)
    println(v12)
    val mSwap12 = QuantumGate.SWAP(3, 0)
    println(mSwap12)
    val mCNOT02 = QuantumGate(3, QuantumGate.CNOT, 0, 2)
    println(mCNOT02)
    val v = new QuantumState(3)
    println(v)
    v.transform(QuantumGate(3, QuantumGate.pauliX, 2))
    println(v)
    v.transform(QuantumGate(3, QuantumGate.CNOT, 0, 2))
    println(v)
    v.transform(QuantumGate(3, QuantumGate.pauliX, 0))
    println(v)
    v.transform(QuantumGate(3, QuantumGate.CNOT, 0, 2))
    println(v)
  }

}
