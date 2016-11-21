/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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

package scalismo.faces.utilities

import scala.annotation.tailrec

object LanguageUtilities {
  /** time an operation, print result with message */
  def time[@specialized A](msg: String)(f: => A): A = {
    val (v, time) = timed(f)
    println(msg + ": " + time + "s")
    v
  }

  /** time operation */
  def timed[@specialized A](f: => A): (A, Double) = {
    val start = System.nanoTime()
    val retVal = f
    val stop = System.nanoTime()
    (retVal, (stop - start) / 1e9)
  }

  /** iterate a function */
  @tailrec
  def iterate[@specialized A](initial: A, iterations: Int)(f: A => A): A = {
    if (iterations > 0)
      iterate(f(initial), iterations - 1)(f)
    else
      initial
  }

  /** mutable object initialization as a single block */
  def withMutable[A](obj: A)(f: A => Unit) = {
    f(obj)
    obj
  }
}
