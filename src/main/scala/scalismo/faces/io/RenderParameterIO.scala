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

package scalismo.faces.io

import java.io._
import java.util.Scanner

import scalismo.faces.io.renderparameters.RenderParameterJSONFormat
import scalismo.faces.parameters.RenderParameter
import scalismo.faces.utils.ResourceManagement
import spray.json._

import RenderParameterJSONFormat.renderParameterFormat

import scala.util.Try

object RenderParameterIO {

  /** write a RenderParameter to a file */
  def write(parameter: RenderParameter, file: File): Try[Unit] = writeWithFormat[RenderParameter](parameter, file)

  /** read RenderParameter from a json file */
  def read(file: File): Try[RenderParameter] = readWithFormat[RenderParameter](file)

  /** read a Parameter type from a json file, needs the json format, e.g. RenderParameterJSONFormatLegacy._ */
  def readWithFormat[A](file: File)(implicit format: RootJsonFormat[A]): Try[A] = Try(
    readASTFromStream(new FileInputStream(file)).convertTo[A]
  )

  /** write a parameter type to a file, needs a json format for the type, e.g. RenderParameterJSONFormatLegacy._ */
  def writeWithFormat[A](parameter: A, file: File)(implicit format: RootJsonFormat[A]): Try[Unit] = Try(
    writeASTToStream(parameter.toJson, new FileOutputStream(file))
  )

  /**
   * read a parameter type from a path in a nested json file, path format: field1/field2/field3
   *
   * @param file
   *   File to read from
   * @param path
   *   path in the json tree, format: field1/field2/field3
   */
  def readWithPath[A](file: File, path: String)(implicit format: JsonFormat[A]): Try[A] = {
    readFromStreamWithPath[A](new FileInputStream(file), path)
  }

  /**
   * write a parameter to a file at a given path
   *
   * @param parameter
   *   Parameter to write
   * @param file
   *   File to write
   * @param path
   *   path inside json, format: field1/field2/field3
   */
  def writeWithPath[A](parameter: A, file: File, path: String)(implicit format: JsonFormat[A]): Try[Unit] = {
    writeToStreamWithPath[A](parameter, new FileOutputStream(file), path)
  }

  /** write a RenderParameter to a file */
  def writeToStream(parameter: RenderParameter, stream: OutputStream): Try[Unit] =
    writeToStreamWithFormat[RenderParameter](parameter, stream)

  /** read RenderParameter from a json file */
  def readFromStream(stream: InputStream): Try[RenderParameter] = readFromStreamWithFormat[RenderParameter](stream)

  /** read a Parameter type from a json file, needs the json format, e.g. RenderParameterJSONFormatLegacy._ */
  def readFromStreamWithFormat[A](stream: InputStream)(implicit format: RootJsonFormat[A]): Try[A] = Try(
    readASTFromStream(stream).convertTo[A]
  )

  /** write a parameter type to a file, needs a json format for the type, e.g. RenderParameterJSONFormatLegacy._ */
  def writeToStreamWithFormat[A](parameter: A, stream: OutputStream)(implicit format: RootJsonFormat[A]): Try[Unit] =
    Try(writeASTToStream(parameter.toJson, stream))

  /**
   * read a parameter type from a path in a nested json file, path format: field1/field2/field3
   *
   * @param stream
   *   File to read from
   * @param path
   *   path in the json tree, format: field1/field2/field3
   */
  def readFromStreamWithPath[A](stream: InputStream, path: String)(implicit format: JsonFormat[A]): Try[A] = {
    val json = readASTFromStream(stream)
    readFromASTWithPath[A](json, path)
  }

  /**
   * write a parameter to a file at a given path
   *
   * @param parameter
   *   Parameter to write
   * @param stream
   *   Stream to write
   * @param path
   *   path inside json, format: field1/field2/field3
   */
  def writeToStreamWithPath[A](parameter: A, stream: OutputStream, path: String)(implicit
    format: JsonFormat[A]
  ): Try[Unit] = Try {
    val fullAST = writeToASTWithPath(parameter, path)
    writeASTToStream(fullAST, stream)
  }

  /**
   * read a parameter type from a path in a nested json file, path format: field1/field2/field3
   *
   * @param json
   *   JsValue containing full ASTree
   * @param path
   *   path in the json tree, format: field1/field2/field3
   */
  def readFromASTWithPath[A](json: JsValue, path: String)(implicit format: JsonFormat[A]): Try[A] = Try {
    val version = RenderParameterJSONFormat.versionString(json)
    val fieldNames: IndexedSeq[String] = path.split("/").filter(_.nonEmpty).toIndexedSeq
    // traverse the json AST along the given path
    val jsonField = fieldNames.foldLeft(json) { (jsValue, fieldName) => jsValue.asJsObject.fields(fieldName) }
    jsonField.convertTo[A]
  }

  /**
   * write a parameter to a file at a given path
   *
   * @param parameter
   *   Parameter to write
   * @param path
   *   path inside json, format: field1/field2/field3
   */
  def writeToASTWithPath[A](parameter: A, path: String)(implicit format: JsonFormat[A]): JsValue = {
    val parameterAST = parameter.toJson
    val fieldNames: IndexedSeq[String] = path.split("/").filter(_.nonEmpty).toIndexedSeq
    // create composite, nested tree along path
    val fullAST = fieldNames.reverse.foldLeft(parameterAST) { (compositeTree, fieldName) =>
      JsObject((fieldName, compositeTree))
    }
    fullAST
  }

  /** parse json from stream as abstract syntax tree (spray's JsValue) */
  def readASTFromStream(stream: InputStream): JsValue = {
    ResourceManagement.using(new Scanner(stream).useDelimiter("\\A")) { scanner =>
      val string = if (scanner.hasNext()) scanner.next() else ""
      string.parseJson
    }
  }

  /** write an abstract syntax tree (spray's JsValue) to a file (pretty printed) */
  def writeASTToStream(jasonAST: JsValue, stream: OutputStream): Unit = {
    ResourceManagement.using(new PrintWriter(stream)) { f =>
      f.print(jasonAST.prettyPrint)
    }
  }
}
