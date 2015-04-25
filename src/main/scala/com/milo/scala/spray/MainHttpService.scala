package com.milo.scala.spray

import com.bmj.ics.net.rest.RestIcsApi
import com.bmj.bsi.restapi.ics.net.BSIRestIcsApi

import com.milo.scala.model.AccountImmutable

import scala.util.{Failure, Success}

import com.milo.scala.adaptor.RestServiceAdaptor
import spray.routing.{Directives, HttpServiceActor}
import spray.json._
import Directives._
import DefaultJsonProtocol._

/**
 * Created by milori on 22/01/2015.
 */
class MainHttpService extends HttpServiceActor {


  val ics = new BSIRestIcsApi("ics.internal.bmjgroup.com", 6700)
  val adaptor = new RestServiceAdaptor(ics)


  def receive = runRoute {

    accountByIdRoute ~ deleteByIdRoute ~ updateByIdRoute ~ createAccount ~ accountByEmail
    //~ accountPostConvert
  }



  val accountByEmail = path("accounts") {
    parameter('email) { email =>
      get {
        import com.milo.scala.model.AccountJsonProtocol._
        val accounts =
          adaptor.accountByEmail(email).map(AccountJsonFormat.write(_))
        complete(accounts.toJson.compactPrint)
      }
    }

  }

val deleteByIdRoute = path("account" / PathElement ) { accountId =>

  delete {
    println("******** delete")

    import com.milo.scala.model.AccountJsonProtocol._
    val accountDelete =
      adaptor.deleteById(accountId) match
      {
        case Success (success) => success .map (com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(_))
        case Failure (exp) =>  "{\"error\":\"" + exp.getMessage + "\"}"
      }

    complete(accountDelete.toString)
  }
}

  val accountByIdRoute = path("account" / PathElement ) { accountId =>

    get {
      println("******** get")
      import com.milo.scala.model.AccountJsonProtocol._
      val account =
        adaptor.accountById(accountId).map(AccountJsonFormat.write(_))

      complete(account.get.toJson.compactPrint)
    }
  }

val updateByIdRoute = path("account" / PathElement ) { accountId =>

  post {
    println("account update")
    entity(as[String]) { account =>

      val fObject = JsonParser(account)

      import com.milo.scala.model.AccountJsonProtocol._

      val obj = fObject.convertTo[AccountImmutable]

      val z = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.read(fObject)

      val json = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(z)

      val result = adaptor.updateAccount(z, accountId) match {

        case Success(success) => com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(success)
        case Failure(exp) => "{\"error\":\"" + exp.getMessage + "\"}"
      }
      complete(result.toString)
    }
  }
}



  val accountById = path("account" / PathElement ) { accountId =>

      get {
        println("******** get")
        import com.milo.scala.model.AccountJsonProtocol._
        val account =
          adaptor.accountById(accountId).map(AccountJsonFormat.write(_))
        complete(account.get.toJson.compactPrint)
      } ~
        delete {
        println("******** delete")

        import com.milo.scala.model.AccountJsonProtocol._
        val accountDelete =
          adaptor.deleteById(accountId) match
          {
            case Success (success) => success .map (com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(_))
            case Failure (exp) =>  "{\"error\":\"" + exp.getMessage + "\"}"
          }

        complete(accountDelete.toString)
      } ~
        post {
        println("account update")
        entity(as[String]) { account =>

          val fObject = JsonParser(account)

          import com.milo.scala.model.AccountJsonProtocol._

          val obj = fObject.convertTo[AccountImmutable]

          val z = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.read(fObject)

          val json = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(z)

          val result = adaptor.updateAccount(z,accountId) match {

            case Success(success) => com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(success)
            case Failure(exp) => "{\"error\":\"" + exp.getMessage + "\"}"
          }
          complete(result.toString)
        }
      }
  }




  def updateAccount = path("account" / PathElement) {
       accountId =>
      post {
        println("account update")
        entity(as[String]) { account =>

          val fObject = JsonParser(account)
          import com.milo.scala.model.AccountJsonProtocol._

          val obj = fObject.convertTo[AccountImmutable]

          val z = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.read(fObject)

          val json = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(z)

          val result = adaptor.updateAccount(z,accountId) match {

            case Success(success) => com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(success)
            case Failure(exp) => "{\"error\":\"" + exp.getMessage + "\"}"
          }
          complete(result.toString)
        }


      }
  }



  val createAccount = path("account") {

    post {

      entity(as[String]) { account =>

        val fObject = JsonParser(account)
        import com.milo.scala.model.AccountJsonProtocol._

        val obj = fObject.convertTo[AccountImmutable]
        val z = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.read(fObject)
        println(z)


        val json = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(z)

        val result = adaptor.createNewAccount(z) match
        {

          case Success (success) => com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(success)
          case Failure (exp) =>  "{\"error\":\"" + exp.getMessage + "\"}"
        }

        complete(result.toString)
      }


    }

  }


/**
  def deleteById = path("accounts" / PathElement ) { accountId =>

    delete {
      import com.milo.scala.model.AccountJsonProtocol._
      val account =
        adaptor.deleteById(accountId) match
        {
          case Success (success) => com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(success)
          case Failure (exp) =>  "{\"error\":\"" + exp.getMessage + "\"}"
        }

      complete(account.get.toJson.compactPrint)
    }
  }
**/




  def routeAccounts = path("accounts") {
    parameter('email) { email =>
      get {
        import com.milo.scala.model.AccountJsonProtocol._
        val accounts =  //adaptor.accountByEmail(email).map(adaptor.convertToJson(_))
          adaptor.accountByEmail(email).map(AccountJsonFormat.write(_))
        //val jsSeq = JsList
        complete(accounts.toJson.compactPrint)
      }
    }

  }

  def routeCreate = path("account") {
    post {
      entity(as[String]) {
        content: String =>
          complete(String.valueOf(content))
      }
    }
  }


  def routeCreate2 = path("account2") {
    post {
      entity(as[String]) {
        content: String =>
          complete(String.valueOf(content))
      }
    }
  }

  def updateAccount2 = path("accounts") {

    post {
      //println("account create")
      entity(as[String]) { account =>

        val fObject = JsonParser(account)
        import com.milo.scala.model.AccountJsonProtocol._

        val obj = fObject.convertTo[AccountImmutable]

        //println (obj)
        val z = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.read(fObject)
        println(z)


        val json = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(z)

        //println(json)

        //println (z.toJson)

        val result = adaptor.createNewAccount(z) match
        {

          case Success (success) => com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(success)
          case Failure (exp) =>  "{\"error\":\"" + exp.getMessage + "\"}"
        }
        //}
        //}/*foreach(z=>{println("account or exception " + z)}) /*.map(i =>complete(i)).get*/

        complete(result.toString)
      }


    }

  }



  def postAccount = path("accountPost") {

    post {

      entity(as[String]) { account =>

        val fObject = JsonParser(account)
        import com.milo.scala.model.AccountJsonProtocol._

        val obj = fObject.convertTo[AccountImmutable]


        val z = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.read(fObject)
        println(z)


        val json = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(z)

       val result = adaptor.createNewAccount(z) match
      {
          case Success (success) => com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(success)
          case Failure (exp) =>  "{\"error\":\"" + exp.getMessage + "\"}"
        }
        complete(result.toString)
      }


    }

  }

  def accountPostConvert = path("accountPostConvert") {

    import com.milo.scala.model.AccountJsonProtocol._

    implicit val jFormat = AccountJsonFormat
    post {
      println("account create II")
      entity(as[String]) { account =>

        //val fObject = JsonParser(account)


        //val z = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.read(fObject)
        //println(z)
        //val json = com.milo.scala.model.AccountJsonProtocol.AccountJsonFormat.write(z)

        println(account)

        complete(account.toString)
      }


    }

  }

}