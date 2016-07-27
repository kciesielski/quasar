/*
 *  ____    ____    _____    ____    ___     ____
 * |  _ \  |  _ \  | ____|  / ___|  / _/    / ___|        Precog (R)
 * | |_) | | |_) | |  _|   | |     | |  /| | |  _         Advanced Analytics Engine for NoSQL Data
 * |  __/  |  _ <  | |___  | |___  |/ _| | | |_| |        Copyright (C) 2010 - 2013 SlamData, Inc.
 * |_|     |_| \_\ |_____|  \____|   /__/   \____|        All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the
 * GNU Affero General Public License as published by the Free Software Foundation, either version
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
 * the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <http://www.gnu.org/licenses/>.
 *
 */
package com.precog.niflheim

import blueeyes._
import com.precog.common._
import scalaz._
import quasar.precog.TestSupport._

class V1CookedBlockFormatSpecs extends CookedBlockFormatSpecs {
  val format = V1CookedBlockFormat
}

case class VersionedCookedBlockFormatSpecs() extends CookedBlockFormatSpecs {
  val format = VersionedCookedBlockFormat(Map(1 -> V1CookedBlockFormat))
}

trait CookedBlockFormatSpecs extends Specification with ScalaCheck with SegmentFormatSupport {
  def format: CookedBlockFormat

  implicit val arbFile = Arbitrary(for {
    parts <- Gen.listOfN(3, Gen.identifier map { part =>
      part.substring(0, math.min(part.length, 5))
    })
  } yield new File(parts.mkString("/", "/", ".cooked")))

  implicit val arbSegmentId = Arbitrary(genSegmentId)

  "cooked block format" should {
    "round trip empty segments" in {
      surviveRoundTrip(format)(CookedBlockMetadata(999L, 0, new Array[(SegmentId, File)](0)))
    }

    "round trip simple segments" in {
      surviveRoundTrip(format)(CookedBlockMetadata(999L, 1,
          Array(SegmentId(1234L, CPath("a.b.c"), CLong) -> new File("/hello/there/abc.cooked"))
      ))
    }

    "roundtrip arbitrary blocks" in false.pendingUntilFixed /*{
      check { files: List[(SegmentId, File)] =>
        surviveRoundTrip(format)(CookedBlockMetadata(999L, files.length, files.toArray))
      }.set(maxDiscarded -> 2000)
    }*/
  }

  //def surviveRoundTrip(format: CookedBlockFormat)(segments0: Array[(SegmentId, File)]) = {
  def surviveRoundTrip(format: CookedBlockFormat)(segments0: CookedBlockMetadata) = {
    val out = new InMemoryWritableByteChannel
    format.writeCookedBlock(out, segments0) must beLike {
      case Success(_) =>
        val in = new InMemoryReadableByteChannel(out.toArray)
        format.readCookedBlock(in) must beLike {
          case Success(segments1) =>
            segments1 must_== segments0
        }
    }
  }
}
