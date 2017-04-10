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

package scalismo.faces.utils

import java.io.Closeable

import scala.io.Source
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

/** basic loan pattern resource management methods */
object ResourceManagement {

  /** block with ensured cleanup function */
  def withCleanup[T](cleanup: => Unit)(f: => T): T = {
    try {
      f
    } finally {
      cleanup
    }
  }

  /** using block: let-like loan structure with possible cleanup function */
  def usingSource[R](source: Source, after: Source => Unit = { s: Source => s.close() })(block: Source => R): R = {
    try {
      block(source)
    } finally {
      after(source)
    }
  }

  /** using block: let-like loan structure with possible cleanup function, defaults to close */
  def using[T <: Closeable, R](obj: => T, after: T => Unit = { t: T => t.close() })(block: T => R): R = {
    val o = obj
    try {

      block(o)
    } finally {
      after(o)
    }
  }

  /** using block: let-like structure with possible cleanup function, aware of Try */
  def usingTry[T <: Closeable, R](obj: => Try[T], after: T => Unit = { t: T => t.close() })(block: T => Try[R]): Try[R] = {
    val o: Try[T] = try {
      obj
    } catch {
      case NonFatal(e) => Failure(e)
    }
    o.flatMap { res =>
      try {
        block(res)
      } finally {
        after(res)
      }
    }
  }

  /** using block: let-like structure with possible cleanup function, aware of Option */
  def usingOption[T <: Closeable, R](obj: => Option[T], after: T => Unit = { t: T => t.close() })(block: T => Option[R]): Option[R] = {
    val o: Option[T] = try {
      obj
    } catch {
      case NonFatal(e) => None
    }
    o.flatMap { res =>
      try {
        block(res)
      } finally {
        after(res)
      }
    }
  }

}
