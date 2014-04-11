package org.learningconcurrency
package reactive



import rx.lang.scala._



object CreatingObservables {

  def main(args: Array[String]) {
    val o = Observable.items(1, 2, 3)
    o.subscribe(n => println(n))
  }

}



