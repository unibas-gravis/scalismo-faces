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

import java.net.URI
import scalismo.faces.parameters.*
import upickle.default.{read, writeJs as write}

import scala.util.{Failure, Success, Try}

object RenderParameterJSONFormatV4 extends RenderParameterRootJsonFormat {

  override val version = "V4.0"

  import scalismo.faces.io.renderparameters.fromats.RenderParameterJSONFormats._

  val pureRpsMapper: upickle.default.ReadWriter[RenderParameter] = upickle.default.macroRW
  implicit val rpsMapper: upickle.default.ReadWriter[RenderParameter] = upickle.default
    .readwriter[ujson.Value]
    .bimap[RenderParameter](
      rps => {
        val json = write(rps)(pureRpsMapper)
        json("version") = version
        json
      },
      json => {
        Try {
          json("version").str
        } match {
          case Success(version) =>
            if (version != "V4.0")
              throw IllegalArgumentException(s"V4 json reader expects V4.0 json file, got: $version")
          case Failure(e) =>
            throw IllegalArgumentException(s"V4 json reader expects V4.0 json file, got: $e")
        }
        read[RenderParameter](json)(pureRpsMapper)
      }
    )
}
