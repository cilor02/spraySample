package com.milo.scala.adaptor

import java.text.SimpleDateFormat

import com.bmj.bsi.restapi.dto.AccountDTO
import com.bmj.bsi.restapi.exception.{RestAPIBadParameterException, RestAPIResourceNotFoundException}
import com.bmj.ics.model.{IdentityType, Identity}
import com.bmj.ics.model.impl.{IdImpl, IdentityImpl}
import com.bmj.ics.net.IcsApi
import com.bmj.ics.xml.PropertyName
import com.ics.LovFileProcessor
import com.milo.scala.model.{AccountImmutable}
import org.json4s.JObject

import scala.util.{Failure, Try,Success}

//import org.json4s.JsonAST.JObject
import spray.json.{JsString, JsObject}
import scala.collection.JavaConverters._
import scala.collection.JavaConversions.mapAsScalaMap


/**
 * Created by milori on 27/01/2015.
 */
class RestServiceAdaptor (icsApi: IcsApi)
{

  val lovFileProcessor = LovFileProcessor ("http://lov-ics.internal.bmjgroup.com/lov-ics.xml")

  val validProfessions = lovFileProcessor.allProfessions

  val validSpecialties = lovFileProcessor.specialties

  val validCountries = lovFileProcessor.allCountries

  def accountByEmail(email:String):List[AccountImmutable]=
    {
      val icsSession = icsApi.createSession()
      val identities = icsApi.findIdentitiesByPropertyValue(icsSession, "emailAddress", email.toLowerCase())
      identities.asScala.filter(_.getPropertyValue("state").equalsIgnoreCase("enabled")).toList.map( convertToAccountDTO(_))
    }



  def createNewAccount(account: AccountImmutable): Try[AccountImmutable] =
  {

    Try(
    {
      //accountId.map(acc => {icsApi.findIdentity(icsSession, new IdImpl(acc)) match { case null => case id if isIdentityEnabled(id) => throw new RestAPIBadParameterException(acc + " already exists")}})

      validateAccount(account) match {
        case Failure(exp) => throw(exp)
        case Success (success) =>
      }
//check for existing active account with same email
      val icsSession = icsApi.createSession()
      account.email.foreach( email =>
      {

          icsApi.findIdentitiesByPropertyValue(
            icsSession,
            PropertyName.EMAIL_ADDRESS,
            email.toLowerCase()) match { case null => case identities => identities.asScala.find(isIdentityEnabled(_)).foreach(x=>{ throw new RestAPIBadParameterException("email " + email + " already in use") }) }
      }
      )

      val identity = accountDTOtoIdentity(account);
      //account.professions.map( )

      account
    }
    )
  }


  def validateAccount(account : AccountImmutable):Try[AccountImmutable] =
  {
    Try({
      account.email match {
        case None => throw new RestAPIBadParameterException("Email must be specified")
        case Some (x) =>
      }

      account.country match {
        case None => throw new RestAPIBadParameterException("Country must be specified")
        case Some (country) if !validCountries.contains( country) => throw new RestAPIBadParameterException(country + " not recognised")
        case Some (country) =>
      }

      account.professions.map (profs => { profs.filter{(!validProfessions.contains (_))}} ) match  /*((x,y) => {if(lovFileProcessor.allProfessions.contains (y))   y })*/
      {
        case Some (invProfs) if invProfs.length > 0 => throw new RestAPIBadParameterException(" Invalid Professions " + invProfs.mkString(","))
        case Some (invProfs) =>
        case None =>
      }

      account.specialties.map (sp => { sp.filter{(!validSpecialties.contains (_))}} ) match  /*((x,y) => {if(lovFileProcessor.allProfessions.contains (y))   y })*/
      {
        case Some (invSpecialties) if invSpecialties.length > 0 => throw new RestAPIBadParameterException(" Invalid Specialties " + invSpecialties.mkString(","))
        case Some (invSpecialties) =>
        case None =>
      }

      account.graduationYear match {
        case Some (year) => {val numYear = Integer.parseInt(year); println("numYear " +numYear); if(numYear < 1900 || numYear > 3000) throw  new RestAPIBadParameterException("Invalid Graduation Year "
          + numYear)}
        case None =>
      }

      account
    }
    )
  }

  def isIdentityEnabled(id:Identity):Boolean = {println("identity _______" + id); "enabled".equalsIgnoreCase(id.getPropertyValue(PropertyName.STATE))}

  def updateAccount(account: AccountImmutable): Try[AccountImmutable] = {

    val icsSession = icsApi.createSession()
    val accountId = account.accountId

    val identity =
      accountId.map(acc => icsApi.findIdentity(icsSession, new IdImpl(acc)))

    Try(
    {
      identity.foreach(id => {
        if (id == null || !isIdentityEnabled(id)) {
          throw new RestAPIResourceNotFoundException("Id " + accountId
            + " cannot be found.")
        }

      }
      )
      account
    }
    )

  }
    /*
Try(


)
val identity = accountDTOtoIdentity(account)

*/
    //account.professions.map( )


    //    identity
    //  }


    def isListNulls(s: List[String]): () => Option[List[String]] = {
      () => s match {
        case null => None
        case list => Some(s)
      }
    }
    def isNulls(s: String): () => Option[String] = {
      () => s match {
        case null => None
        case str => Some(s)
      }
    }

    def accountDTOtoIdentity(accountImmutable: AccountImmutable): Unit = {
      val identity = new IdentityImpl();


      accountImmutable.accountId.foreach(s => identity.setId(new IdImpl(s)))

      identity.setProperty(PropertyName.TYPE, IdentityType.INDIVIDUAL.getValue());
      accountImmutable.givenName.foreach(identity.setProperty("forename", _))
      accountImmutable.familyName.map(identity.setProperty("familyName", _))
      accountImmutable.email.map(identity.setProperty("emailAddress/0", _))
      accountImmutable.bmaNumber.map(identity.setProperty("bmaMemberNumber", _))
      accountImmutable.country.map(identity.setProperty("country/0", _))
      accountImmutable.city.map(identity.setProperty("town/0", _))
      accountImmutable.province.map(identity.setProperty("county/0", _))
      accountImmutable.organisation.map(identity.setProperty("placeOfWork", _))
      accountImmutable.postalCode.map(identity.setProperty("postcode/0", _))
      accountImmutable.address.map(identity.setProperty("address1/0", _))
      accountImmutable.title.map(identity.setProperty(PropertyName.TITLE, _))
      accountImmutable.telephone.map(identity.setProperty("phoneDirect/0", _))
      accountImmutable.graduationYear.map(identity.setProperty("qualificationDate" + "-01-01", _))

      val sdf = new SimpleDateFormat("yyyy-MM-dd");

      identity.setProperty("registrationLastUpdatedDate", sdf.format(new java.util.Date()));
      identity.setProperty("emailMatrixLastUpdatedDate", sdf.format(new java.util.Date()));

      accountImmutable.specialties.map(_.filter(sp => lovFileProcessor.specialties.contains(sp)).zipWithIndex.map(x => (PropertyName.MEDICAL_SPECIALTY + "/" + x._2, x._1)).toMap)
      val icsProfFields = accountImmutable.professions.map(_.zipWithIndex.flatMap(x => lovFileProcessor.processProfession(x._1, x._2)))
      icsProfFields.foreach(_.foreach(p => identity.setProperty(p._1, p._2)))


      println(identity)


    }

    def convertToAccountDTO(id: Identity): AccountImmutable = {

      println(lovFileProcessor.processProfession("137", 0))
      println(lovFileProcessor.processProfession("90", 1))
      println(lovFileProcessor.processProfession("83", 2))
      println(lovFileProcessor.processProfession("160", 3))

      val address = for {
        add1 <- isNulls(id.getPropertyValue("address1/0"))()
        add2 <- isNulls(id.getPropertyValue("address2/0"))()
        add3 <- isNulls(id.getPropertyValue("address3/0"))()
        add4 <- isNulls(id.getPropertyValue("address4/0"))()
      } yield add1 + add2 + add3 + add4


      val mapProps = mapAsScalaMap(id.getProperties)

      val professions = mapProps.keySet.filter(_.startsWith("professionTier")).map(mapProps.get(_)).map({ case Some(s) => s case None =>}).filter(validProfessions.contains(_))

      val profStrings = professions.map(_.asInstanceOf[String])

      val specialties = (0 to 3).toList.map(x => {
        id.getPropertyValue("medicalSpecialty/" + x)
      }).filter(sp => {
        sp != null && validSpecialties.contains(sp)
      })

      println(professions)
      println(specialties)
      AccountImmutable(
        isNulls(id.getId.getValue)(),
        isNulls(id.getPropertyValue("titlePrefix"))(),
        isNulls(id.getPropertyValue("forename"))(),
        isNulls(id.getPropertyValue("familyName"))(),
        isNulls(id.getPropertyValue("emailAddress/0"))(),
        address,
        //isNulls(id.getPropertyValue("address1/0") + id.getPropertyValue("address2/0") + id.getPropertyValue("address3/0") + id.getPropertyValue("address4/0"))(),
        isNulls(id.getPropertyValue("bmaMemberNumber"))(),
        isNulls(id.getPropertyValue("country/0"))(),
        isNulls(id.getPropertyValue("town/0"))(),
        isNulls(id.getPropertyValue("county/0"))(),
        isNulls(id.getPropertyValue("placeOfWork"))(),
        isNulls(id.getPropertyValue("postcode/0"))(),
        isNulls(id.getPropertyValue("phoneDirect/0"))(),
        isNulls(id.getPropertyValue("qualificationDate"))(),
        isListNulls(specialties)(),
        isListNulls(profStrings.toList)()

      )

    }

    def convertToJson(accountImmutable: AccountImmutable): JsObject = {
      JsObject(
        "accountId" -> JsString(accountImmutable.accountId.getOrElse("")),
        "forename" -> JsString(accountImmutable.givenName.getOrElse("")),
        "familyName" -> JsString(accountImmutable.familyName.getOrElse("")),
        "emailAddress" -> JsString(accountImmutable.email.getOrElse("")),
        "address" -> JsString(accountImmutable.address.getOrElse(""))
      )
    }

  }
  /*def convertMultipleToJson(accountImmutableList: List[AccountImmutable]): JsObject =
  {
    JsObject (
      "accountId" -> JsString (accountImmutable.accountId.getOrElse("")),
      "forename" -> JsString (accountImmutable.givenName.getOrElse("")),
      "accountId" -> JsString (accountImmutable.familyName.getOrElse("")),
      "emailAddress" -> JsString (accountImmutable.email.getOrElse("")),
      "address" -> JsString (accountImmutable.address.getOrElse(""))
    )
  }*/
//}
