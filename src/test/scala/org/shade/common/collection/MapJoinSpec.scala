/*
 *  Copyright 2014 James Shade
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.shade.common.collection

import org.scalatest.{ShouldMatchers, WordSpec}

class MapJoinSpec extends WordSpec with ShouldMatchers {

  import MapDecorators.MapJoin

  "An outer join" should {

    def join(v1: Option[String], v2: Option[Int]) = "*" + v1.getOrElse("N1") + "*" + v2.getOrElse("N2") + "*"

    "return an empty map if both input maps are empty" in {
      val m1 = Map.empty[String, String]
      val m2 = Map.empty[String, Int]

      m1.outerJoin(m2)(join) shouldBe empty
    }

    "return a map containing all entries from the first map joined with None if the second map is empty" in {

      val m1 = Map[String, String]("A" -> "a", "B" -> "b")
      val m2 = Map.empty[String, Int]

      m1.outerJoin(m2)(join) shouldBe Map[String, String](
        "A" -> "*a*N2*",
        "B" -> "*b*N2*")
    }

    "return a map containing all entries from the second map joined with None if the first map is empty" in {

      val m1 = Map.empty[String, String]
      val m2 = Map[String, Int]("A" -> 1, "B" -> 2)

      m1.outerJoin(m2)(join) shouldBe Map[String, String](
        "A" -> "*N1*1*",
        "B" -> "*N1*2*"
      )
    }

    "return a map containing all entries of both maps joined together where there are no overlapping keys" in {

      val m1 = Map[String, String]("A" -> "a", "B" -> "b")
      val m2 = Map[String, Int]("C" -> 1, "D" -> 2)

      m1.outerJoin(m2)(join) shouldBe Map[String, String](
        "A" -> "*a*N2*",
        "B" -> "*b*N2*",
        "C" -> "*N1*1*",
        "D" -> "*N1*2*")
    }

    "return a map containing the joined entries of both maps where there are overlapping keys" in {

      val m1 = Map[String, String]("A" -> "a", "B" -> "b", "C" -> "c")
      val m2 = Map[String, Int]("B" -> 1, "C" -> 2, "D" -> 3)

      m1.outerJoin(m2)(join) shouldBe Map[String, String](
        "A" -> "*a*N2*",
        "B" -> "*b*1*",
        "C" -> "*c*2*",
        "D" -> "*N1*3*")
    }
  }
}
