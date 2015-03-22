package com.ics

import scala.xml.XML

/**
 * Created by milori on 04/03/2015.
 */
class LovFileProcessor (url: String)
{

  val xml = XML.load(url)
  val tier3Properties =  (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier3").flatMap (  y => (y \ "lovproperty" ))
  val tier3Professions = tier3Properties.map(  x => ( x \ "@value").text )
  val groupingsAtLevel2 = tier3Properties.map ( v =>( v \ "@dependsParentValue").text)

  val tier2Properties = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier2").flatMap (  y => (y \ "lovproperty" )).filter(  x => !groupingsAtLevel2.contains(( x \ "@value" ).text) )
  val tier2Professions =  tier2Properties.map(  x => ( x \ "@value").text )
  val groupingsAtLevel1 = tier2Properties.map ( v =>( v \ "@dependsParentValue").text)

  val tier1Properties = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier1").flatMap (  y => (y \ "lovproperty" )).filter(  x => !groupingsAtLevel1.contains(( x \ "@value" ).text) )
  val tier1Professions =  tier1Properties.map(  x => ( x \ "@value").text )

  def level3Associations = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier3").flatMap (  y => (y \ "lovproperty" )).map( v =>(( v \ "@value").text,( v \ "@dependsParentValue").text)).toMap

  def level2Associations =  (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier2").flatMap (  y => (y \ "lovproperty" )).map( v =>(( v \ "@value").text,( v \ "@dependsParentValue").text)).toMap

  def allProfessions = tier1Professions ++ tier2Professions ++ tier3Professions

  def specialties = (xml \ "lovitem").filter ( att => ( att \ "@id").text == "medicalSpecialty").flatMap (  y => (y \ "lovproperty" )).map(  x => ( x \ "@value").text )


  def allCountries = (xml \ "lovitem").filter ( att => ( att \ "@id").text == "country").flatMap( y =>(y \ "lovproperty")).map( x=> (x \ "@value").text)


  def processProfession (profession: String, index :Int): List[(String,String)] =
  {
    if (tier3Professions.contains(profession))
    {

      val icsTier3Key = ("professionTier3/" + index,profession)

      val level2Node = level3Associations.get(profession).get

      val icsTier2Key = ("professionTier2/" + index,level2Node)

      val level1Node = level2Associations.get(level2Node).get

      val icsTier1Key = ("professionTier1/" + index,level1Node)
      List (icsTier1Key, icsTier2Key, icsTier3Key)
    }else if (tier2Professions.contains(profession))
    {
      val icsTier2Key = ("professionTier2/" + index,profession)
      val level1Node = level2Associations.get(profession).get
      val icsTier1Key = ("professionTier1/" + index,level1Node)
      List (icsTier1Key, icsTier2Key)
    }else if (tier1Professions.contains(profession))
    {
      val icsTier1Key = ("professionTier1/" + index,profession)
      List (icsTier1Key)
    }else
    {
      List()
    }
  }

}




object LovFileProcessor
{



  def apply (url:String): LovFileProcessor =
  {
    new LovFileProcessor(url)
  }
}