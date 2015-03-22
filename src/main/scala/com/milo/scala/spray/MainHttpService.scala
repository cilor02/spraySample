package com.milo.scala.spray

import com.bmj.bsi.restapi.dto.AccountDTO
import com.bmj.bsi.restapi.service.adaptor.ICSServiceAdaptor
import com.bmj.ics.net.IcsApi
import com.bmj.ics.net.rest.RestIcsApi
import com.bmj.ics.xml.PropertyName
import com.milo.scala.model.AccountImmutable

import scala.util.{Failure, Success}

//import com.milo.json.{TestPayload, AccountJsonFormat}
import com.milo.scala.adaptor.RestServiceAdaptor
import spray.json.JsArray
import spray.routing.HttpServiceActor
import scala.collection.JavaConverters._
import spray.json._
import DefaultJsonProtocol._

/**
 * Created by milori on 22/01/2015.
 */
class MainHttpService extends HttpServiceActor {

  val ics = new RestIcsApi("ics.internal.bmjgroup.com", 6700)
  val adaptor = new RestServiceAdaptor(ics)


  def receive = runRoute {

    routeCreate ~ postPayload ~ postAccount ~ routeAccounts
    //~ accountPostConvert
  }

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

  def postPayload = path("payload") {

    post {
      println("payload")
      entity(as[String]) { payload =>

        val fObject = JsonParser(payload)
        import com.milo.json.TestPayloadProtocol._

        val z = PayloadJsonFormat.read(fObject)
        //println(z)
        //val body = fObject.convertTo[TestPayload]
        complete(String.valueOf(payload.toJson))
      }
    }
  }

  def postAccount = path("accountPost") {

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