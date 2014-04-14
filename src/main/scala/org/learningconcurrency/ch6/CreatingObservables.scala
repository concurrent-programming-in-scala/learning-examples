package org.learningconcurrency
package ch6



import rx.lang.scala._



object CreatingObservables extends App {

    val o = Observable.items(1, 2, 3)
    o.subscribe(n => println(n))

}



