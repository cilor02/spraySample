package com.milo.scala.model

import spray.httpx.SprayJsonSupport
import spray.json._

//import org.json4s._
/**
 * Created by milori on 25/01/2015.
 */


case class AccountImmutable
(

val accountId:Option[String] ,
val title : Option[String],
val givenName:Option[String] ,
val familyName:Option[String] ,
val email:Option[String],
val address:Option[String] ,
val bmaNumber:Option[String] ,
val country:Option[String] ,
val city:Option[String] ,
val province:Option[String],
val organisation:Option[String],
val postalCode:Option[String] ,
val telephone:Option[String],
val graduationYear:Option[String] ,
val specialties: Option[List[String]],
val professions: Option[List[String]]

  )

object AccountJsonProtocol extends DefaultJsonProtocol with SprayJsonSupport
{
  implicit object AccountJsonFormat extends JsonFormat[AccountImmutable]
  {

    def write(acc: AccountImmutable) = {
      val fieldorNull = ((z:Option[String]) =>z match { case Some(x:String) => JsString (x) case None => JsNull })
      val listorNull = (z:Option[List[String]]) =>z match { case Some(x:List[String]) => JsArray(x.toVector.map(JsString(_))) case None => JsNull }

      JsObject(
        "accountId" -> fieldorNull(acc.accountId),
        "title" -> fieldorNull(acc.title),
        "givenName" -> fieldorNull(acc.givenName),
        "familyName" -> fieldorNull(acc.familyName),
        "email" -> fieldorNull (acc.email),
        "address" -> fieldorNull(acc.address),
        "bmaNumber" -> fieldorNull(acc.bmaNumber),
       "country" -> fieldorNull(acc.country),
        "city" -> fieldorNull(acc.city),
        "province" -> fieldorNull(acc.province),
        "organisation" -> fieldorNull(acc.organisation),
        "postalCode" -> fieldorNull(acc.postalCode),
        "telephone" -> fieldorNull(acc.telephone),
        "graduationYear" -> fieldorNull(acc.graduationYear),
        "specialties" -> listorNull (acc.specialties),
        "professions" -> listorNull (acc.professions)
      )
    }

    def read(value: JsValue ) = {
      (value.asJsObject.fields.get("accountId"),
        value.asJsObject.fields.get("title"),
        value.asJsObject.fields.get("givenName"),
        value.asJsObject.fields.get("familyName"),
        value.asJsObject.fields.get("email"),
        value.asJsObject.fields.get("address"),
        value.asJsObject.fields.get("bmaNumber"),
        value.asJsObject.fields.get("country"),
        value.asJsObject.fields.get("city"),
        value.asJsObject.fields.get("province"),
        value.asJsObject.fields.get("organisation"),
        value.asJsObject.fields.get("postalCode"),
        value.asJsObject.fields.get("telephone"),
        value.asJsObject.fields.get("graduationYear"),
        value.asJsObject.fields.get("specialties"),
        value.asJsObject.fields.get("professions")
        ) match {
        case (accountId,
        title,
        givenName,
        familyName,
        email,
        address,
        bmaNumber,
        country,
        city,
        province,
        organisation,
        postalCode,
        telephone,
        graduationYear,
        specialties,
        professions
          ) => AccountImmutable(accountId.map(_.asInstanceOf[JsString].value),
          title.map(_.asInstanceOf[JsString].value),
          givenName.map(_.asInstanceOf[JsString].value),
          familyName.map(_.asInstanceOf[JsString].value),
          email.map(_.asInstanceOf[JsString].value),
          address.map(_.asInstanceOf[JsString].value),
          bmaNumber.map(_.asInstanceOf[JsString].value),
          country.map(_.asInstanceOf[JsString].value),
          city.map(_.asInstanceOf[JsString].value),
          province.map(_.asInstanceOf[JsString].value),
          organisation.map(_.asInstanceOf[JsString].value),
          postalCode.map(_.asInstanceOf[JsString].value),
          telephone.map(_.asInstanceOf[JsString].value),
          graduationYear.map(_.asInstanceOf[JsString].value),
          specialties.map(_.asInstanceOf[JsArray].convertTo[List[String]]),
          professions.map(_.asInstanceOf[JsArray].convertTo[List[String]]))
      }

    }

    }

}




