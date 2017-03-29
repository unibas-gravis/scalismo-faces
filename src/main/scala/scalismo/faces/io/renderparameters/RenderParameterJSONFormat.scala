/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package scalismo.faces.io.renderparameters

import scalismo.faces.parameters.RenderParameter
import spray.json._

/**
  * default JSON reader and writer for RenderParameter
  * versioned reader, writer uses default format
  * */
object RenderParameterJSONFormat extends RenderParameterJSONFormatV3 {

  val version1 = RenderParameterJSONFormatLegacy
  val version2 = RenderParameterJSONFormatV2
  val version3 = RenderParameterJSONFormatV3
  val version4 = RenderParameterJSONFormatV4

  val defaultFormat = RenderParameterJSONFormatV4

  def versionString(json: JsValue): Option[String] = {
    val fields = json.asJsObject(s"expected RenderParameter object, got: $json").fields

    fields.get("version") match {
      case Some(JsString(v)) => Some(v)
      case _ => None
    }
  }

  /** parse a RenderParameter from the JSON tree, selects proper version */
  def readRenderParameter(json: JsValue): RenderParameter = json.convertTo[RenderParameter]

  /** construct JSON serialization of a RenderParameter, uses default format */
  def writeRenderParameter(param: RenderParameter): JsValue = param.toJson

  override implicit val renderParameterFormat: RootJsonFormat[RenderParameter] = new RootJsonFormat[RenderParameter]{
    def readLegacy(json: JsValue): RenderParameter = {
      RenderParameterJSONFormatLegacy.renderParameterFormat.read(json)
    }

    override def read(json: JsValue): RenderParameter = {
      val version = versionString(json)
      version match {
        case None => readLegacy(json)
        case Some("") => readLegacy(json)
        case Some("legacy") => readLegacy(json)
        case Some(RenderParameterJSONFormatLegacy.version) => readLegacy(json)
        case Some(RenderParameterJSONFormatV2.version) => RenderParameterJSONFormatV2.renderParameterFormat.read(json)
        case Some(RenderParameterJSONFormatV3.version) => RenderParameterJSONFormatV3.renderParameterFormat.read(json)
        case Some(RenderParameterJSONFormatV4.version) => RenderParameterJSONFormatV4.renderParameterFormat.read(json)
        case Some(vstr) => throw new DeserializationException("unknown version: " + vstr)
      }
    }

    override def write(obj: RenderParameter): JsValue = defaultFormat.renderParameterFormat.write(obj)
  }
}