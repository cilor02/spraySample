package com.milo.scala.adaptor

import java.text.SimpleDateFormat

import com.bmj.bsi.restapi.dto.AccountDTO
import com.bmj.bsi.restapi.exception.{RestAPIBadParameterException, RestAPIResourceNotFoundException}
import com.bmj.ics.model.{IdentityType, Identity}
import com.bmj.ics.model.impl.{IdImpl, IdentityImpl}
import com.bmj.ics.net.IcsApi
import com.bmj.bsi.restapi.ics.net.BSIRestIcsApi
import com.bmj.ics.net.rest.RestIcsApi

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
class RestServiceAdaptor (icsApi: BSIRestIcsApi)
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


  def accountById(id:String):Option[AccountImmutable]=
  {
    val icsSession = icsApi.createSession()
    val icsIdResult = icsApi.findIdentity(icsSession,new IdImpl(id)) match { case icsId @ identity => Some(icsId) case null => None}
    icsIdResult.filter (_.getPropertyValue("state").equalsIgnoreCase("enabled")).map (convertToAccountDTO (_))
  }


  def deleteById(id:String):Try[Option[AccountImmutable]]=
  {
    val icsSession = icsApi.createSession()
    val icsIdResult = icsApi.findIdentity(icsSession,new IdImpl(id)) match { case icsId @ identity => Some(icsId) case null => None}

    val idForDelete = icsIdResult.filter (_.getPropertyValue("state").equalsIgnoreCase("enabled"))

    Try(
    {
      idForDelete.foreach(id => {
        if (id == null || !isIdentityEnabled(id)) {
          throw new RestAPIResourceNotFoundException("Id " + id
            + " cannot be found.")
        }

        id.setProperty("state","disabled")


        println("id -----------" + id.getPropertyValue("state"))


        icsApi.deleteIndividualIdentity(icsSession,id)
      }
      )
      idForDelete.map(convertToAccountDTO(_))
    }
    )
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


      //icsApi.createIndividualIdentity(icsSession,identity)
      icsApi.createIndividualShellAccount(icsSession,identity)
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

  def updateAccount(account: AccountImmutable, accountId: String): Try[AccountImmutable] = {

    validateAccount(account) match {
      case Failure(exp) => throw(exp)
      case Success (success) =>
    }
    val icsSession = icsApi.createSession()
    //account.accountId = Some(accountId)

    val identity = icsApi.findIdentity(icsSession, new IdImpl(accountId)) match {case i:Identity => Some(i) case _ => None }

    Try(
    {
      identity.foreach(id => {
        if (id == null || !isIdentityEnabled(id)) {
          throw new RestAPIResourceNotFoundException("Id " + accountId
            + " cannot be found.")
        }

        val updAcc = account.copy(accountId = Some(accountId))
        val identityUpd = accountDTOtoIdentity(updAcc);

        icsApi.updateIndividualIdentity(icsSession,identityUpd)
      }
      )
      account
    }
    )

  }

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

    def accountDTOtoIdentity(accountImmutable: AccountImmutable): Identity = {
      val identity = new IdentityImpl();


      accountImmutable.accountId.foreach(s => identity.setId(new IdImpl(s)))

      identity.setProperty(PropertyName.TYPE, IdentityType.INDIVIDUAL.getValue());
      accountImmutable.givenName.foreach(identity.setProperty("forename", _))
      accountImmutable.familyName.foreach(identity.setProperty("familyName", _))
      accountImmutable.email.foreach(identity.setProperty("emailAddress/0", _))
      accountImmutable.bmaNumber.foreach(identity.setProperty("bmaMemberNumber", _))
      accountImmutable.country.foreach(identity.setProperty("country/0", _))
      accountImmutable.country.foreach(identity.setProperty("primaryCountry", _))

      accountImmutable.city.foreach(identity.setProperty("town/0", _))
      accountImmutable.province.foreach(identity.setProperty("county/0", _))
      accountImmutable.organisation.foreach(identity.setProperty("placeOfWork", _))
      accountImmutable.postalCode.foreach(identity.setProperty("postcode/0", _))
      accountImmutable.address.foreach(identity.setProperty("address1/0", _))
      accountImmutable.title.foreach(identity.setProperty(PropertyName.TITLE, _))
      accountImmutable.telephone.foreach(identity.setProperty("phoneDirect/0", _))
      accountImmutable.graduationYear.foreach(identity.setProperty("qualificationDate" + "-01-01", _))


      identity.setProperty("primaryContact/0", "true")
      val sdf = new SimpleDateFormat("yyyy-MM-dd");

      identity.setProperty("registrationLastUpdatedDate", sdf.format(new java.util.Date()));
      identity.setProperty("emailMatrixLastUpdatedDate", sdf.format(new java.util.Date()));

      val icsSpecialtyFields = accountImmutable.specialties.map(_.filter(sp => lovFileProcessor.specialties.contains(sp)).zipWithIndex.map(x => (PropertyName.MEDICAL_SPECIALTY + "/" + x._2, x._1)))
      icsSpecialtyFields.foreach(_.foreach(p => identity.setProperty(p._1, p._2)))

      val icsProfFields = accountImmutable.professions.map(_.zipWithIndex.flatMap(x => lovFileProcessor.processProfession(x._1, x._2)))
      icsProfFields.foreach(_.foreach(p => identity.setProperty(p._1, p._2)))

      identity
      //println(identity)


    }

    def convertToAccountDTO(id: Identity): AccountImmutable = {

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

