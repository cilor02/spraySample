package com.milo.scala.spray

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.util.Timeout
import scala.concurrent.duration._
import akka.pattern.ask
import spray.can.Http


/**
 * Created by milori on 24/01/2015.
 */
object Boot extends App{
  implicit val system = ActorSystem("mike-ilori")
  val service = system.actorOf(Props[MainHttpService],"request-router")

  implicit val timeout = Timeout  (5.seconds)
  IO(Http) ? Http.Bind(service,interface = "localhost",port = 8081)
}
