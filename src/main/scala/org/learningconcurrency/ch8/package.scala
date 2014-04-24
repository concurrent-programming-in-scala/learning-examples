package org.learningconcurrency



import akka.actor.ActorSystem
import com.typesafe.config._



package object ch8 {
  
  lazy val ourSystem = ActorSystem("OurExampleSystem")

  def remotingConfig(port: Int) = ConfigFactory.parseString(s"""
akka {
  actor.provider = "akka.remote.RemoteActorRefProvider"
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "127.0.0.1"
      port = $port
    }
  }
}
  """)

  def remotingSystem(name: String, port: Int) = ActorSystem(name, remotingConfig(port))

}


