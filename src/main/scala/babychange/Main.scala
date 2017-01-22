package babychange

import akka.http.scaladsl.Http

import scala.io.StdIn

object Main extends App with RestApi {

  implicit val executionContext = system.dispatcher

  val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080)

//  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
//  StdIn.readLine() // let it run until user presses return
//  bindingFuture
//    .flatMap(_.unbind()) // trigger unbinding from the port
//    .onComplete(_ => system.terminate()) // and shutdown when done
}
