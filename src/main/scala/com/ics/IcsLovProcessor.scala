package com.ics

import scala.io.Source
import scala.xml.XML

/**
 * Created by milori on 02/03/2015.
 */
object IcsLovProcessor extends App{

//val f = Source.fromURL("http://lov-ics.internal.bmjgroup.com/lov-ics.xml").mkString


  val xml = XML.load("http://lov-ics.internal.bmjgroup.com/lov-ics.xml")

//println ((xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier3"))

  val tier3Professions = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier3").flatMap (  y => (y \ "lovproperty" )).map(  x => ( x \ "@value").text )

  val tier3Properties =  (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier3").flatMap (  y => (y \ "lovproperty" ))

  val groupingsAtLevel2 = tier3Properties.map ( v =>( v \ "@dependsParentValue").text)
//val tier3Profs = (xml \\ "lovitem").filter ( z =>   (z \\ "@id" ) ==  "professionTier2" )

  val tier2Properties = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier2").flatMap (  y => (y \ "lovproperty" )).filter(  x => !groupingsAtLevel2.contains(( x \ "@value" ).text) )

  val tier2Professions =  tier2Properties.map(  x => ( x \ "@value").text )

  val groupingsAtLevel1 = tier2Properties.map ( v =>( v \ "@dependsParentValue").text)

  val tier1Properties = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier1").flatMap (  y => (y \ "lovproperty" )).filter(  x => !groupingsAtLevel1.contains(( x \ "@value" ).text) )

  val tier1Professions =  tier1Properties.map(  x => ( x \ "@value").text )

  val buildChain = (x:String) => { (tier1Professions ++ tier2Professions ++ tier3Professions).contains(x)  }

  val tier3 = (x:String) => {tier3Professions.contains(x)}

  val associationsLevel3 = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier3").flatMap (  y => (y \ "lovproperty" )).map( v =>(( v \ "@value").text,( v \ "@dependsParentValue").text))
  val associationsLevel2 = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier2").flatMap (  y => (y \ "lovproperty" )).map( v =>(( v \ "@value").text,( v \ "@dependsParentValue").text))
  val associationsLevel1 = (xml \\ "lovitem").filter( z => ( z \ "@id").text == "professionTier1").flatMap (  y => (y \ "lovproperty" )).map( v =>(( v \ "@value").text,( v \ "@dependsParentValue").text))


  val specialties = (xml \ "lovitem").filter ( att => ( att \ "@id").text == "medicalSpecialty").flatMap (  y => (y \ "lovproperty" )).map(  x => ( x \ "@value").text )




 // println (tier3Professions)
 // println (tier2Professions)
 // println (tier1Professions)
 // println(groupingsAtLevel2.toSet)
  println (specialties)

  println(associationsLevel1)
  println(associationsLevel2)
  println(associationsLevel3)



}
