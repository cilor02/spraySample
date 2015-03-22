package com.milo.json

import spray.httpx.SprayJsonSupport
import spray.json._

/**
 * Created by milori on 04/02/2015.
 */
case class TestPayload(f1:Option[String],f2:Option[String],f3:Option[String],f4:Option[String])

object TestPayloadProtocol extends DefaultJsonProtocol with SprayJsonSupport
{
  implicit object PayloadJsonFormat extends JsonFormat[TestPayload]
  {

    def write(testPayload: TestPayload) = JsObject     (
      "f1" -> JsString(testPayload.f1.getOrElse(null)),
      "f2" ->JsString(testPayload.f2.getOrElse(null)),
      "f3" ->JsString(testPayload.f3.getOrElse(null)),
      "f4" ->JsString(testPayload.f4.getOrElse(null))
     )

    def read(value: JsValue ) = {
      (value.asJsObject.fields.get("f1"),
      value.asJsObject.fields.get("f2"),
      value.asJsObject.fields.get("f3"),
      value.asJsObject.fields.get("f4")) match {
        case (f1,f2,f3,f4) => TestPayload(f1.map(_.toString()),f2.map(_.toString()),f3.map(_.toString()),f4.map(_.toString()))
      }
/*
      value.asJsObject.getFields("f1", "f2", "f3", "f4") match {
        case Seq(JsString(f1), JsString(f2), JsString(f3), JsString(f4)) => TestPayload(f1, f2, f3, f4)
        case _ => throw new DeserializationException("testPayload expected")
      }
      */
    }
  }
}

