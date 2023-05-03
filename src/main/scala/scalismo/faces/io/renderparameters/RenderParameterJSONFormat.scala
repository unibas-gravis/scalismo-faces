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

import ujson.Value
import upickle.default.{read, writeJs as write, ReadWriter}

import scala.util.{Success, Try}

/**
 * default JSON reader and writer for RenderParameter versioned reader, writer uses default format
 */
object RenderParameterJSONFormat extends RenderParameterRootJsonFormat {

  val version1 = RenderParameterJSONFormatLegacy.rpsMapper
  val version2 = RenderParameterJSONFormatV2.rpsMapper
  val version3 = RenderParameterJsonFormatV3.rpsMapper
  val version4 = RenderParameterJSONFormatV4.rpsMapper

  val defaultFormat: upickle.default.ReadWriter[RenderParameter] = version4

  def versionString(json: Value): Option[String] = {
    Try {
      json("version").str
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  /** parse a RenderParameter from the JSON tree, selects proper version */
  def readRenderParameter(json: Value): RenderParameter = read[RenderParameter](json)

  /** construct JSON serialization of a RenderParameter, uses default format */
  def writeRenderParameter(param: RenderParameter): Value = write(param)

  implicit override val rpsMapper: ReadWriter[RenderParameter] = upickle.default
    .readwriter[ujson.Value]
    .bimap[RenderParameter](
      rps => write(rps)(defaultFormat),
      json => {
        val version = versionString(json)
        version match {
          case None                                          => read(json)(version1)
          case Some("")                                      => read(json)(version1)
          case Some("legacy")                                => read(json)(version1)
          case Some(RenderParameterJSONFormatLegacy.version) => read(json)(version1)
          case Some(RenderParameterJSONFormatV2.version)     => read(json)(version2)
          case Some(RenderParameterJsonFormatV3.version)     => read(json)(version3)
          case Some(RenderParameterJSONFormatV4.version)     => read(json)(version4)
          case Some(vstr) => throw new IllegalArgumentException("unknown version: " + vstr)
        }
      }
    )
}
