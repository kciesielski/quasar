/*
 * Copyright 2014–2017 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package quasar.physical.rdbms.query

import quasar.contrib.pathy.AFile
import quasar.fp.free._
import quasar.qscript._
import quasar.physical.rdbms.query.Planner.RdbmsState

import doobie.util.fragment.Fragment
import matryoshka.AlgebraM
import org.specs2.scalaz.DisjunctionMatchers
import pathy.Path.{dir, file, rootDir}
import scalaz._
import scalaz.concurrent.Task

class PlannerSpec
    extends quasar.Qspec
    with QScriptHelpers
    with DisjunctionMatchers {

//  import Planner._

  sequential

  val sr = Planner.shiftedread[Task]

  private def runTest[A, S[_]](f: => Free[S, A])(implicit S: S :<: Task): A = {
    f.foldMap(injectNT[S, Task]).unsafePerformSync
  }

  "Planner" should {
    "shiftedReadFile" in {
      runTest {
        val compile: AlgebraM[RdbmsState[Task, ?], Const[ShiftedRead[AFile], ?], Fragment] = sr.plan()

        val afile: AFile = rootDir </> dir("irrelevant") </> dir("test") </> file("data.json")

        val program: RdbmsState[Task, Fragment] = compile(Const(ShiftedRead(afile, ExcludeId)))
        program.run.map(result => result must beRightDisjunction.like {
          case queryFragment =>
            1 must_=== (1)
        })
      }
    }
  }
}
